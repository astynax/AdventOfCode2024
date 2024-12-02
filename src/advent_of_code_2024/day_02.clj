(ns advent-of-code-2024.day-02
  (:require [clojure.java.io :as io]))

(def input
  (for [l (clojure.string/split-lines
           (slurp (io/resource "day-02.input")))
        :let [words (clojure.string/split l #"\s+")
              nums (mapv parse-long words)]]
    nums))

(defn safe? [nums]
  (let [ds (map - nums (rest nums))]
    (and (or (every? pos? ds)
             (every? neg? ds))
         (every? #(<= 1 (abs %) 3) ds))))

(defn skips [xs]
  (for [i (range (count xs))]
    (concat (take i xs) (rest (drop i xs)))))

(defn tolerable-safe? [nums]
  (or (safe? nums)
      (some safe? (skips nums))))

(def solution1 (count (filter safe? input)))

(def solution2 (count (filter tolerable-safe? input)))
