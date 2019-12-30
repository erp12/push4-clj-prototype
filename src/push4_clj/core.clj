(ns push4-clj.core
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :as pp]
            [push4-clj.expression :as expr]
            [push4-clj.spec-utils :as su]
            [push4-clj.dag :as dag]
            [push4-clj.push :as push]))


(def number?-spec
  (s/spec number?))


(def int?-spec
  (s/spec int?))


(def seq?-spec
  (s/spec seq?))


(def simple-numeric-fn-dfs
  (expr/make-dfs [number?-spec number?-spec] number?-spec))


(def expressions
  {:+      (expr/fn->expression '+ + simple-numeric-fn-dfs)
   :-      (expr/fn->expression '- - simple-numeric-fn-dfs)
   :*      (expr/fn->expression '* * simple-numeric-fn-dfs)
   :/      (expr/fn->expression '/ / simple-numeric-fn-dfs)
   :int    (expr/fn->expression 'int int (expr/make-dfs [number?-spec] int?-spec))
   :take   (expr/fn->expression 'take take (expr/make-dfs [int?-spec seq?-spec] seq?-spec))
   :count  (expr/fn->expression 'count count (expr/make-dfs [seq?-spec] (s/spec nat-int?)))
   :2      (expr/lit->expression 2)
   :10     (expr/lit->expression 10)
   :my-seq (expr/sym-expression 'my-seq seq?-spec 0)})


(def spec-dominance-map
  (su/spec-dominance (expr/expressions->spec-set (vals expressions))))


(def take-first-half-genome
  (map expressions
       [:2 :my-seq :count :/ :int :my-seq :take]))


(defn -main
  []
  (let [program (push/compile-to-dag take-first-half-genome
                                     seq?-spec
                                     spec-dominance-map)]
    (pp/pprint program)
    (println)
    (pp/pprint (dag/eval-dag program [[:A :B]]))
    (pp/pprint (dag/eval-dag program [[:A :B :C :D]]))
    (pp/pprint (dag/eval-dag program [[]]))
    (println)
    (pp/pprint (dag/dag->code program))
    (println)
    (pp/pprint (dag/dag->defn 'take-first-half ['my-seq] program))))
