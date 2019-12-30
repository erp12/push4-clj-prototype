(ns push4-clj.expression
  (:require [clojure.spec.alpha :as s]))


(s/def ::expr-name symbol?)  ;; @TODO: Refactor to expr-symbol
(s/def ::return-spec s/spec?)
(s/def ::arg-specs (s/coll-of s/spec? :kind vector?))
(s/def ::fn fn?)
(s/def ::value some?)
(s/def ::position nat-int?)


(s/def ::fn-expr
  (s/keys :req [::expr-name ::return-spec ::arg-specs ::fn]))


(s/def ::lit-expr
  (s/keys :req [::value]))


(s/def ::sym-expr
  (s/keys :req [::expr-name ::return-spec ::position]))


(s/def ::expression
  (s/or :fn ::fn-expr
        :lit ::lit-expr
        :sym ::sym-expr))


(defn make-expression
  [& {:as args}]
  {:post [(s/valid? ::expression %)]}
  (loop [remaining [::fn-expr ::lit-expr ::sym-expr]]
    (let [spec (s/get-spec (first remaining))]
      (cond
        (empty? remaining)
        (throw (Exception. "Bad expression definition."))

        (s/valid? spec args)
        (s/conform spec args)

        :else
        (recur (rest remaining))))))


(defn expression-arity
  [expression]
  {:pre [(s/valid? ::expression expression)]}
  (count (::arg-specs expression)))


; Deconstructed Function Spec
(s/def ::dfs (s/keys :req [::arg-specs ::return-spec]))


(defn make-dfs
  [arg-specs ret-spec]
  {:pre  [(s/valid? ::arg-specs arg-specs)
          (s/valid? ::return-spec ret-spec)]
   :post [(s/valid? ::dfs %)]}
  {::arg-specs   arg-specs
   ::return-spec ret-spec})


(defn fn->expression
  [name fn dfs]
  {:pre  [(s/valid? ::dfs dfs)]
   :post [(s/valid? ::expression %)]}
  (make-expression ::expr-name (symbol name)
                   ::return-spec (::return-spec dfs)
                   ::arg-specs (::arg-specs dfs)
                   ::fn fn))


(defn lit->expression
  [value]
  (make-expression ::value value))


(defn sym-expression
  [name spec position]
  (make-expression ::expr-name name ::return-spec spec ::position position))


(defn expressions->spec-set
  [expressions]
  {:pre  [(s/valid? (s/coll-of ::expression) expressions)]
   :post [(s/valid? (s/coll-of s/spec? :kind set?) %)]}
  (set
    (mapcat (fn [expr]
              (cond
                (s/valid? ::fn-expr expr)
                (conj (::arg-specs expr) (::return-spec expr))

                (s/valid? ::sym-expr expr)
                [(::return-spec expr)]

                :else
                []))
            expressions)))
