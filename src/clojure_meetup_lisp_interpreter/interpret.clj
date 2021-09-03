(ns clojure-meetup-lisp-interpreter.interpret
  (:refer-clojure :exclude [eval apply]))

(def state
  (atom {;; arithmetic
         '+ {:type :builtin :function +}
         '- {:type :builtin :function -}
         '* {:type :builtin :function *}
         '/ {:type :builtin :function /}
         ;; list operations
         'cons {:type :builtin :function cons}
         'first {:type :builtin :function first}
         'rest {:type :builtin :function rest}}))
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

;;; predicates to distinguish between kinds of expressions in the
;;; language and to destructure them.

(defn self-evaluating? [expression]
  (or (number? expression)
      (nil? expression)))

(defn lambda-expression? [expression]
  (and (seq? expression)
       (= (count expression) 3)
       (= (first expression) 'fn)
       (seq? (second expression))))

(defn application? [expression] (and (seq? expression) (not-empty expression)))
(defn application-function [expression] (first expression))
(defn application-arguments [expression] (rest expression))

(defn definition? [expression]
  (and (seq? expression)
       (= (count expression) 3)
       (= (first expression) 'def)
       (symbol? (second expression))))
(defn definition-variable [expression] (second expression))
(defn definition-value [expression] (nth expression 2))

(defn macro-definition? [expression]
  (and (seq? expression)
       (= (count expression) 4)
       (= (first expression) 'defmacro)
       (symbol? (second expression))
       (seq? (nth expression 2))))
(defn macro-definition-variable [expression] (second expression))
(defn macro-definition-arglist [expression] (nth expression 2))
(defn macro-definition-body [expression] (nth expression 3))

(defn quasiquoted? [expression]
  (and (seq? expression)
       (= (count expression) 2)
       (= (first expression) 'quasiquote)))
(defn unquoted? [expression]
  (and (seq? expression)
       (= (count expression) 2)
       (= (first expression) 'unquote)))
(defn unquote-splicing? [expression]
  (and (seq? expression)
       (= (count expression) 2)
       (= (first expression) 'unquote-splicing)))

;;; eval, apply, and eval-quasiquoted, which are mutually recursive
(declare apply)
(declare eval)

(defn eval-quasiquoted [expression environment]
  (cond
    ;; (quasiquote e) where e is not a list
    (not (seq? expression)) expression
    ;; (quasiquote (unquote e))
    (unquoted? expression) (eval (second expression) environment)
    ;; (quasiquote (unquote-splicing e)) is not allowed
    (unquote-splicing? expression)
    (throw (ex-info "cannot splice outside of a list"
                    {:expression expression}))
    ;; (quasiquote ((unquote-splicing e)))
    (and (= 1 (count expression)) (unquote-splicing? (first expression)))
    (eval (first expression) environment)
    ;; (quasiquote (e))
    (= 1 (count expression))
    (list (eval-quasiquoted (first expression) environment))
    ;; (quasiquote ((unquote-splicing e) ...))
    (unquote-splicing? (first expression))
    (concat (eval (first expression) environment)
            (eval-quasiquoted (rest expression) environment))
    ;; (quasiquote (e ...))
    :else (cons (eval-quasiquoted (first expression) environment)
                (eval-quasiquoted (rest expression) environment))))

(defn eval [expression environment]
  (cond
    ;; self-evaluating expressions
    (self-evaluating? expression) expression
    ;; symbols
    (symbol? expression) (lookup environment expression)
    ;; lambdas
    (lambda-expression? expression) {:type :closure
                                     :function expression
                                     :environment environment}
    ;; definitions
    (definition? expression)
    (do (swap! state assoc
               (definition-variable expression)
               (eval (definition-value expression) environment))
        nil)
    ;; macro definitions
    (macro-definition? expression)
    (do (swap! state assoc
               (macro-definition-variable expression)
               {:type :macro
                :function (list 'fn (macro-definition-arglist expression)
                                (macro-definition-body expression))
                :environment environment}))
    ;; quasiquoted expressions
    (quasiquoted? expression) (eval-quasiquoted (second expression) environment)
    ;; application of builtins, user-defined functions, or macros
    (application? expression)
    #_
    (apply (eval (first expression) environment)
           (map #(eval % environment) (rest expression)))
    (apply (application-function expression)
           (application-arguments expression)
           environment)
    :else (throw (ex-info "unknown expression to evaluate"
                          {:expression expression}))))

(defn apply [function-expression arguments-expression environment]
  (let [{:keys [function type]
         closure-environment :environment}
        (eval function-expression environment)]
    (cond
      (= type :builtin)
      (clojure.core/apply function
                          (map #(eval % environment) arguments-expression))
      (= type :closure)
      (let [arglist (second function)
            body (nth function 2)
            provided (count arguments-expression)
            accepted (count arglist)]
        (when (not= provided accepted)
          (throw (ex-info "arity mismatch"
                          {:provided provided :accepted accepted})))
        (eval body
              (merge closure-environment
                     (zipmap arglist
                             (map #(eval % environment) arguments-expression)))))
      (= type :macro)
      (let [arglist (second function)
            body (nth function 2)
            provided (count arguments-expression)
            accepted (count arglist)]
        (when (not= provided accepted)
          (throw (ex-info "arity mismatch"
                          {:provided provided :accepted accepted})))
        (eval (eval body
                    (merge closure-environment
                           (zipmap arglist
                                   arguments-expression)))
              environment))
      :else (throw (ex-info "cannot apply expression"
                            {:expression function-expression})))))

;;; start extending the language using the macro system
(eval
 '(defmacro defn (name args body)
    (quasiquote (def
                  (unquote name)
                  (fn (unquote args)
                    (unquote body)))))
 nil)
(eval
 '(defn inc (x) (+ 1 x))
 nil)

(comment
  (eval '(((fn (x) (fn (y) (+ x y))) 1) 2) nil))

