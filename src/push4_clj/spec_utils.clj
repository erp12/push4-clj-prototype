(ns push4-clj.spec-utils
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]
            [clojure.spec.gen.alpha :as sg]
            [clojure.math.combinatorics :as comb]
            [push4-clj.expression :as expr]))


(defn spec-sample
  [spec size]
  (sg/sample (s/gen spec) size))


(defn all-valid?
  [spec coll]
  (every? #(s/valid? spec %) coll))


(defn spec-dominates-spec?
  [super-spec sub-spec confidence]
  (let [sub-spec-sample (spec-sample sub-spec confidence)]
    (all-valid? super-spec sub-spec-sample)))


(defn spec-dominance
  ([specs]
   (spec-dominance specs 1000))
  ([specs confidence]
   ;; TODO: Consider switching to nested loop to cut down on sample generation.
   (loop [spec-pairs (comb/permuted-combinations specs 2)
          spec->sub-specs {}]
     (if (empty? spec-pairs)
       spec->sub-specs
       (let [[super-spec sub-spec] (first spec-pairs)]
         (recur (rest spec-pairs)
                (if (spec-dominates-spec? super-spec sub-spec confidence)
                  (update spec->sub-specs super-spec (fnil conj #{}) sub-spec)
                  spec->sub-specs)))))))


;; Custom Generators

(def pos?-gen
  (sg/fmap #(Math/abs (if (< (rand) 0.5) (int %) %))
           (sg/double* {:infinite? false :NaN? false :min 1e5 :max 1e5})))
