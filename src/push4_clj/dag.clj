(ns push4-clj.dag
  (:require [clojure.spec.alpha :as s]
            [push4-clj.expression :as expr]))


(s/def ::children (s/coll-of ::dag :kind vector?))
;; @TODO: Add unique ID to each DAG node.
(s/def ::dag (s/keys :req [::expr/expression ::children]))

(defn expression->dag
  ([expression]
   (assert (or (s/valid? ::expr/sym-expr expression)
               (s/valid? ::expr/lit-expr expression)
               (and (s/valid? ::expr/fn-expr expression)
                    (zero? (expr/expression-arity expression))))
           "Cannot create DAG with no children from an expression that requires args.")
   (expression->dag expression []))
  ([expression children]
   {:pre [(s/valid? ::children children)
          (if (s/valid? ::expr/fn-expr expression)
            (= (expr/expression-arity expression) (count children))
            true)]}
   ;; @TODO: Consider checking spec dominance is correct.
   {::expr/expression expression
    ::children        children}))


(defn dag-result-spec
  [dag]
  (::expr/return-spec (::expr/expression dag)))


(defn eval-dag
  [dag registry]
  (let [expression (::expr/expression dag)]
    (cond
      (s/valid? ::expr/fn-expr expression)
      (apply (::expr/fn expression)
             (map #(eval-dag % registry)
                  (::children dag)))

      (s/valid? ::expr/lit-expr expression)
      (::expr/value expression)

      (s/valid? ::expr/sym-expr expression)
      (nth registry (::expr/position expression))

      :else
      (throw (Exception. "Found node with invalid expression.")))))


(defn dag->code
  [dag]
  (let [expression (::expr/expression dag)]
    (cond
      (s/valid? ::expr/fn-expr expression)
      (apply list
             (reduce conj
                     [(::expr/expr-name expression)]
                     (map dag->code (::children dag))))

      (s/valid? ::expr/lit-expr expression)
      (::expr/value expression)

      (s/valid? ::expr/sym-expr expression)
      (::expr/expr-name expression)

      :else
      (do
        (println expression)
        (throw (Exception. "Found node with invalid expression."))))))


(defn dag->defn
  [fn-name arg-names dag]
  `(defn ~fn-name ~arg-names ~(dag->code dag)))
