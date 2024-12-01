(ns advent-of-code-2024.day-01
  (:require [clojure.java.io :as io]))

(def input
  (for [l (clojure.string/split-lines
           (slurp (io/resource "day-01.input")))
        :let [words (clojure.string/split l #"\s+")
              nums (mapv parse-long words)]]
    nums))

(def solution1
  (let [col1 (map first input)
        col2 (map second input)
        diffs (map (fn [n1 n2] (abs (- n1 n2)))
                   (sort col1)
                   (sort col2))]
    (reduce + diffs)))

(def solution2
  (let [col1 (map first input)
        col2 (map second input)
        freqs (frequencies col2)
        diffs (map (fn [n] (* n (get freqs n 0)))
                   col1)]
    (reduce + diffs)))
