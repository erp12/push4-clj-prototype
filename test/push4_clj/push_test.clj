(ns push4-clj.push-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [push4-clj.push :refer :all]
            [push4-clj.dag :as dag]
            [push4-clj.expression :as expr]))


(deftest test-stack-push
  (is (= (stack-push '() :a)
         '(:a)))
  (is (= (stack-push '(:a) :b)
         '(:b :a))))


(deftest test-stack-remove-nth
  (is (= (stack-remove-nth '(:top :middle :bottom) 0)
         '(:middle :bottom)))
  (is (= (stack-remove-nth '(:top :middle :bottom) 2)
         '(:top :middle)))
  (is (= (stack-remove-nth '() 2)
         '())))


(deftest test-stack-pop-top-valid
  (let [int?-spec (s/spec int?)
        number?-spec (s/spec number?)
        lit-2 (dag/expression->dag (expr/lit->expression 2))]
    (testing "valid lit on stack"
      (is (= (stack-pop-top-valid (list lit-2) #{int?-spec number?-spec})
             ['() lit-2])))))
