(ns push4-clj.dag-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [push4-clj.dag :as dag]
            [push4-clj.expression :as expr]))


(deftest test-expression->dag
  (let [int?-spec (s/spec int?)
        float?-spec (s/spec float?)]
    (testing "from literal"
      (is (= (dag/expression->dag (expr/lit->expression 1))
             {::expr/expression {::expr/value 1} ::dag/children []}))
      (is (= (dag/expression->dag (expr/lit->expression "a"))
             {::expr/expression {::expr/value "a"} ::dag/children []}))
      (is (= (dag/expression->dag (expr/lit->expression [:a :b :c]))
             {::expr/expression {::expr/value [:a :b :c]} ::dag/children []})))
    (testing "from symbol"
      (is (= (dag/expression->dag (expr/sym-expression 'x int?-spec 0))
             {::expr/expression {::expr/expr-name   'x
                                 ::expr/return-spec int?-spec
                                 ::expr/position    0}
              ::dag/children    []})))
    (testing "from fn"
      (is (= (dag/expression->dag (expr/fn->expression 'inc inc (expr/make-dfs [int?-spec] int?-spec))
                                  [(dag/expression->dag (expr/lit->expression 0))])
             {::expr/expression {::expr/expr-name   'inc
                                 ::expr/arg-specs   [int?-spec]
                                 ::expr/return-spec int?-spec
                                 ::expr/fn          inc}
              ::dag/children    [{::expr/expression {::expr/value 0}
                                  ::dag/children    []}]}))
      (is (= (dag/expression->dag (expr/fn->expression 'rand rand (expr/make-dfs [] float?-spec)))
             {::expr/expression {::expr/expr-name   'rand
                                 ::expr/arg-specs   []
                                 ::expr/return-spec float?-spec
                                 ::expr/fn          rand}
              ::dag/children    []})))))


(deftest test-dag-result-spec
  (let [int?-spec (s/spec int?)
        inc-expr (expr/fn->expression 'inc inc (expr/make-dfs [int?-spec] int?-spec))]
    (testing "of literal DAG"
      (is (nil? (dag/dag-result-spec (dag/expression->dag (expr/lit->expression 1)))))
      (is (nil? (dag/dag-result-spec (dag/expression->dag (expr/lit->expression :a)))))
      (is (nil? (dag/dag-result-spec (dag/expression->dag (expr/lit->expression ""))))))
    (testing "of symbol DAG"
      (is (= (dag/dag-result-spec (dag/expression->dag (expr/sym-expression 'x int?-spec 0)))
             int?-spec)))
    (testing "of fn DAG"
      (is (= (dag/dag-result-spec (dag/expression->dag inc-expr [(dag/expression->dag (expr/lit->expression 0))]))
             int?-spec)))))
