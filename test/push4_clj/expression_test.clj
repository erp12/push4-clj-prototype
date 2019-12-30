(ns push4-clj.expression-test
  (:require [clojure.test :refer :all]
            [push4-clj.expression :as expr]
            [clojure.spec.alpha :as s]))


(deftest test-make-expression
  (let [number?-spec (s/spec number?)
        string?-spec (s/spec string?)
        foo-fn (fn [] "foo")]
    (testing "making lit expression"
      (is (= (expr/lit->expression 0) {::expr/value 0}))
      (is (= (expr/lit->expression "") {::expr/value ""}))
      (is (= (expr/lit->expression {:map "literal"}) {::expr/value {:map "literal"}})))
    (testing "making sym expression"
      (is (= (expr/sym-expression 's string?-spec 0)
             {::expr/expr-name 's ::expr/return-spec string?-spec ::expr/position 0})))
    (testing "making fn expression"
      (is (= (expr/fn->expression 'foo foo-fn (expr/make-dfs [] string?-spec))
             {::expr/expr-name 'foo ::expr/arg-specs [] ::expr/return-spec string?-spec ::expr/fn foo-fn}))
      (is (= (expr/fn->expression '+ + (expr/make-dfs [number?-spec number?-spec] number?-spec))
             {::expr/expr-name '+
              ::expr/arg-specs [number?-spec number?-spec]
              ::expr/return-spec number?-spec
              ::expr/fn +})))))


(deftest test-expression-arity
  (let [number?-spec (s/spec number?)
        string?-spec (s/spec string?)
        foo-fn (fn [] "foo")]
    (testing "of literal and symbol expressions"
      (is (zero? (expr/expression-arity (expr/lit->expression 0))))
      (is (zero? (expr/expression-arity (expr/lit->expression {:k "v"})))))
    (testing "of fn expressions"
      (is (= (expr/expression-arity (expr/fn->expression '+ + (expr/make-dfs [number?-spec number?-spec] number?-spec))) 2))
      (is (zero? (expr/expression-arity (expr/fn->expression 'foo foo-fn (expr/make-dfs [] string?-spec))))))))


(deftest test-expressions->spec-set
  (let [number?-spec (s/spec number?)
        int?-spec (s/spec int?)
        nat-int?-spec (s/spec nat-int?)
        seq?-spec (s/spec seq?)
        simple-numeric-fn-dfs (expr/make-dfs [number?-spec number?-spec] number?-spec)
        expressions {:+      (expr/fn->expression '+ + simple-numeric-fn-dfs)
                     :-      (expr/fn->expression '- - simple-numeric-fn-dfs)
                     :*      (expr/fn->expression '* * simple-numeric-fn-dfs)
                     :/      (expr/fn->expression '/ / simple-numeric-fn-dfs)
                     :int    (expr/fn->expression 'int int (expr/make-dfs [number?-spec] int?-spec))
                     :take   (expr/fn->expression 'take take (expr/make-dfs [int?-spec seq?-spec] seq?-spec))
                     :count  (expr/fn->expression 'count count (expr/make-dfs [seq?-spec] nat-int?-spec))
                     :2      (expr/lit->expression 2)
                     :10     (expr/lit->expression 10)
                     :my-seq (expr/sym-expression 'my-seq seq?-spec 0)}]
    (is (= (expr/expressions->spec-set (vals expressions))
           #{number?-spec int?-spec nat-int?-spec seq?-spec}))))
