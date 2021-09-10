(ns clojure-meetup-lisp-interpreter.interpret
  (:refer-clojure :exclude [eval apply]))

(defn lookup [environment symbol]
  (if (contains? environment symbol)
    (get environment symbol)
    (throw (ex-info "symbol is not bound"
                    {:symbol symbol :current-environment environment}))))

(defn lambda-expression? [expression]
  (and (list? expression)
       (= (count expression) 3)
       (= (first expression) 'fn)
       (list? (second expression))))
(defn if-expression? [expression]
  (and (list? expression)
       (= (count expression) 4)
       (= (first expression) 'if)))
(defn do-expression? [expression]
  (and (list? expression)
       (= (first expression) 'do)))
(defn application? [expression] (and (list? expression) (not-empty expression)))

(declare apply)

(defn eval [expression environment]
  (cond
    ;; numbers
    (number? expression) expression
    ;; nil
    (nil? expression) expression
    ;; symbols
    (symbol? expression) (lookup environment expression)
    ;; anonymous functions
    (lambda-expression? expression) {:type :closure
                                     :function expression
                                     :environment environment}
    ;; if expressions
    (if-expression? expression)
    (let [[_if test consequent alternative] expression]
      (eval (if-not (nil? (eval test environment)) consequent alternative)
            environment))
    ;; do expressions
    (do-expression? expression)
    (reduce (fn [_ e] (eval e environment)) nil (rest expression))
    ;; applications
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

