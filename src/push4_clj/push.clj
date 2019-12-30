(ns push4-clj.push
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :as pp]
            [push4-clj.expression :as expr]
            [push4-clj.dag :as dag]))


;; @TODO: Consider adding a 'sketch' structure to the state. Use it to improve stack search speed.
(s/def ::stack (s/coll-of ::dag/dag :kind list?))


(defn stack-push
  [stack dag]
  (conj stack dag))


(defn stack-remove-nth
  [stack n]
  (concat (take n stack)
          (drop (inc n) stack)))


(defn stack-pop-top-valid
  [stack specs]
  (loop [remaining stack
         ndx 0]
    (let [next-dag (first remaining)
          next-expr (::expr/expression next-dag)]
      (cond
        ; If no valid expressions found on the stack.
        (empty? remaining)
        [stack :none-valid]

        ; @TODO: Fix and test this logic.
        (or
          ; If lit-expr and the value satisfies any of the specs.
          (and (s/valid? ::expr/lit-expr next-expr)
               (some #(s/valid? % (::expr/value next-expr)) specs))
          ; If other expr and the return spec is one of the specs.
          (and (or (s/valid? ::expr/fn-expr next-expr)
                   (s/valid? ::expr/sym-expr next-expr))
               (contains? (set specs) (dag/dag-result-spec next-dag))))
        [(stack-remove-nth stack ndx) next-dag]

        :else
        (recur (rest remaining)
               (inc ndx))))))


(defn- spec-and-sub-specs
  [spec spec-dominance]
  (set (conj (get spec-dominance spec) spec)))


(defn pop-children
  [stack child-specs spec-dominance]
  (loop [remaining-specs child-specs
         new-stack stack
         children []]
    (if (empty? remaining-specs)
      [new-stack children]
      (let [next-spec (first remaining-specs)
            acceptable-specs (spec-and-sub-specs next-spec spec-dominance)
            [next-stack child] (stack-pop-top-valid new-stack acceptable-specs)]
        (recur (rest remaining-specs)
               next-stack
               (conj children child))))))


(defn compile-to-dag
  [push-code output-spec spec-dominance]
  (loop [remaining push-code
         stack '()]
    (let [next-unit (first remaining)]
      (cond
        ;; If Push compilation is finished, pop the top DAG that produces the desired spec.
        (empty? remaining)
        (second (stack-pop-top-valid stack (spec-and-sub-specs output-spec spec-dominance)))

        ;; If the unit is a lit or sym expression, no arguments are needed so push it to the stack.
        (or (s/valid? ::expr/lit-expr next-unit)
            (s/valid? ::expr/sym-expr next-unit))
        (recur (rest remaining)
               (stack-push stack (dag/expression->dag next-unit)))

        ;; If the unit is a fn expression
        ;; 1) find and pop the child DAGs. If missing any, skip the unit.
        ;; 2) Create a DAG using the expression and children. Push it to the stack.
        (s/valid? ::expr/fn-expr next-unit)
        (let [[new-stack children] (pop-children stack (::expr/arg-specs next-unit) spec-dominance)]
          (recur (rest remaining)
                 (if (some #{:none-valid} children)
                   stack
                   (stack-push new-stack (dag/expression->dag next-unit children)))))

        ; @TODO: Add stack manipulation instructions.
        ; @TODO: Add code manipulation instructions. (ie. dup, swap, rot, reverse, etc).

        :else
        (throw (Exception. "Found invalid Push unit during execution."))))))
