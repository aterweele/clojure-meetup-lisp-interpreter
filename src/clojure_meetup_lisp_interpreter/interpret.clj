(ns clojure-meetup-lisp-interpreter.interpret
  (:refer-clojure :exclude [eval apply]))

(defn lookup [environment symbol]
  (if (contains? environment symbol)
    (first (environment symbol))
    (throw (ex-info "symbol is not bound"
                    {:symbol symbol :current-environment environment}))))

(defn self-evaluating? [form] (number? form))
(defn lambda-form? [form] (= (first form) #_'fn 'λ))

(declare apply)

(defn eval [form environment]
  (cond
    (self-evaluating? form) form
    (symbol? form) (lookup environment form)
    (lambda-form? form) {:type :closure
                         :function form
                         :environment environment}
    (list? form) (apply (eval (first form) environment)
                        (map #(eval % environment) (rest form)))
    :else (throw (ex-info "unknown form to evaluate"
                          {:form form}))))

(defn apply [{:as f :keys [type function environment]} args]
  (condp = type
    :builtin (clojure.core/apply function args)
    :closure (let [[_ arglist body] function
                   provided (count args)
                   accepted (count arglist)]
               (when (not= provided accepted)
                 (throw (ex-info "arity mismatch"
                                 {:provided provided :accepted accepted})))
               (eval body (reduce-kv (fn [environment arg value]
                                       ;; FIXME: we don't need to push
                                       ;; the new value because we
                                       ;; only ever use the top
                                       ;; value. So just "overwrite"
                                       ;; instead.
                                       (update environment arg conj value))
                                     environment
                                     (zipmap arglist args))))))

(def builtin-environment
  {'+ (list {:type :builtin :function +})
   '- (list {:type :builtin :function -})
   '* (list {:type :builtin :function *})
   '/ (list {:type :builtin :function /})})

(comment
  (eval '(((fn (x) (fn (y) (+ x y))) 1) 2) builtin-environment))
