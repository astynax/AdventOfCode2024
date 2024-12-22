(ns advent-of-code-2024.day-22
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :as m]
            [advent-of-code-2024.utils :refer :all]))

(def input (->> (io/resource "day-22.input")
                slurp
                s/split-lines
                (mapv parse-long)))

(def example [1
              10
              100
              2024])

(defn next-secret [^:Integer n]
  (let [n (-> n
              (bit-shift-left 6)
              (bit-xor n)
              (bit-and 0xFFFFFF))
        n (-> n
              (bit-shift-right 5)
              (bit-xor n)
              (bit-and 0xFFFFFF))
        n (-> n
              (bit-shift-left 11)
              (bit-xor n)
              (bit-and 0xFFFFFF))]
    n))

(defn times [n f x] (first (drop n (iterate f x))))

(defn sum-after [n xs]
  (->> xs
       (map #(times n next-secret %))
       (reduce +)))

(defn changes-of [n]
  (let [ones (->> n
                  (iterate next-secret)
                  (take 2000)
                  (map #(int (mod % 10))))
        diffs (map - (rest ones) ones)]
    (map vector
         ones
         (partition 4 1 (lazy-cat '(nil nil nil nil) diffs)))))

(defn most-bananas [nums]
  (->> (for [[i n] (enumerate nums)
             [t c] (drop 4 (changes-of n))]
         [i t c])
       (reduce (fn [acc [i t c]]
                 (let [m (get acc c {})
                       m (assoc m i (get m i t))]
                   (assoc acc c m)))
               {})
       vals
       (map #(reduce + (vals %)))
       (reduce max)))

#_(most-bananas [1 2 3 2024])

(def solution1 (sum-after 2000 input))

(def solution2 (most-bananas input))
