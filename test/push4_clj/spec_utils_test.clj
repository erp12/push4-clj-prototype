(ns push4-clj.spec-utils-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [push4-clj.spec-utils :refer :all]
            [clojure.spec.gen.alpha :as sg]
            [clojure.pprint :as pp]))


(deftest test-sample-valid
  (testing "with atomic spec"
    (let [sample (spec-sample (s/spec number?) 10)]
      (is (= (count sample) 10))
      (is (every? number? sample))))
  (testing "with collection spec"
    (let [sample (spec-sample (s/coll-of (s/spec string?)) 10)]
      (is (= (count sample) 10))
      (is (every? coll? sample))
      (is (every? string? (flatten sample)))))
  (testing "empty-sample"
    (let [sample (spec-sample (s/spec number?) 0)]
      (is (empty? sample)))))


(deftest test-all-valid?
  (let [int?-spec (s/spec int?)
        pos?-spec (s/spec pos?)]
    (testing "with all valid"
      (is (all-valid? int?-spec #{-1 0 1 1000}))
      (is (all-valid? pos?-spec [(/ 1 3) 1e5 0.001])))
    (testing "with some invalid"
      (is (not (all-valid? int?-spec '(-1 (/ 1 3)))))
      (is (not (all-valid? pos?-spec #{0.0 1.1 2.2}))))))


(deftest test-spec-dominates-spec?
  (let [int?-spec (s/spec int?)
        pos?-spec (s/spec pos?)
        pos-int?-spec (s/spec pos-int?)
        nat-int?-spec (s/spec nat-int?)
        confidence (int 1e4)]
    (testing "with dominate spec"
      (is (spec-dominates-spec? int?-spec pos-int?-spec confidence))
      (is (spec-dominates-spec? pos?-spec pos-int?-spec confidence))
      (is (spec-dominates-spec? nat-int?-spec pos-int?-spec confidence)))
    (testing "with non-dominate spec"
      (is (not (spec-dominates-spec? pos?-spec int?-spec confidence)))
      (is (not (spec-dominates-spec? pos-int?-spec nat-int?-spec confidence))))))


(deftest test-spec-dominance
  (let [int?-spec (s/spec int?)
        pos?-spec (s/with-gen (s/spec pos?) (fn [] pos?-gen))
        pos-int?-spec (s/spec pos-int?)
        nat-int?-spec (s/spec nat-int?)]
    (is (= (spec-dominance #{int?-spec pos?-spec pos-int?-spec nat-int?-spec} 1000)
           {int?-spec #{pos-int?-spec nat-int?-spec}
            pos?-spec #{pos-int?-spec}
            nat-int?-spec #{pos-int?-spec}}))))


(deftest test-pos?-gen
  (is (every? pos? (sg/sample pos?-gen 1000))))
