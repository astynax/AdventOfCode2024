(ns advent-of-code-2024.day-25
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn decode [text]
  (->> text
       s/trim
       s/split-lines
       (partition 7 8)
       (mapv (fn [ls]
               (let [nums (replicate (count (first ls)) 0)]
                 (reduce (partial map (fn [n c] (+ (* n 2) (if (= c \#) 1 0))))
                         nums
                         ls))))))

(def input (decode (slurp (io/resource "day-25.input"))))

(def example [(64 126 120 124 120)
              (96 112 64  126 120)
              (63 1   7   3   15)
              (31 15  31  1   7)
              (15 1   7   1   3)])

(defn key? [item]
  (every? #(pos? (bit-and 1 %)) item))

(defn fit? [a b]
  (every? zero? (map bit-and a b)))

(defn fits [items]
  (-> (for [l (filter (complement key?) items)
            k (filter key? items)
            :when (fit? l k)]
        true)
      count))

(def solution1 (fits input))
