(ns clojure-meetup-lisp-interpreter.interpret
  (:refer-clojure :exclude [eval apply]))

(def state (atom {}))
(defn reset-state!
  "Convenience function to clear definitions."
  []
  (reset! state {}))

(defn lookup [environment symbol]
  (cond
    (contains? environment symbol) (get environment symbol)
    (contains? @state symbol) (get @state symbol)
    :else (throw (ex-info "symbol is not bound"
                          {:symbol symbol :current-environment environment}))))

(defn self-evaluating? [expression]
  (or (number? expression)
      (nil? expression)))
(defn lambda-expression? [expression]
  (and (list? expression)
       (= (count expression) 3)
       (= (first expression) 'fn)
       (list? (second expression))))
(defn application? [expression] (and (list? expression) (not-empty expression)))
;; TODO: application-function and application-arguments, maybe
(defn definition? [expression]
  (and (list? expression)
       (= (count expression) 3)
       (= (first expression) 'def)
       (symbol? (second expression))))
(defn definition-variable [expression] (second expression))
(defn definition-value [expression] (nth expression 2))

(declare apply)

(defn eval [expression environment]
  (cond
    (self-evaluating? expression) expression
    (symbol? expression) (lookup environment expression)
    (lambda-expression? expression) {:type :closure
                                     :function expression
                                     :environment environment}
    (definition? expression)
    (do (swap! state assoc
               (definition-variable expression)
               (eval (definition-value expression) environment))
        nil)
    (application? expression)
    (apply (eval (first expression) environment)
           (map #(eval % environment) (rest expression)))
    :else (throw (ex-info "unknown expression to evaluate"
                          {:expression expression}))))

(defn apply [{:as f :keys [type function environment]} args]
  (condp = type
    :builtin (clojure.core/apply function args)
    :closure (let [[_ arglist body] function
                   provided (count args)
                   accepted (count arglist)]
               (when (not= provided accepted)
                 (throw (ex-info "arity mismatch"
                                 {:provided provided :accepted accepted})))
               (eval body (merge environment
                                 (zipmap arglist args))))))

(def builtin-environment
  {'+ {:type :builtin :function +}
   '- {:type :builtin :function -}
   '* {:type :builtin :function *}
   '/ {:type :builtin :function /}})

(comment
  (eval '(((fn (x) (fn (y) (+ x y))) 1) 2) builtin-environment))

