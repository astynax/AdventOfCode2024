(ns advent-of-code-2024.day-11
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :as m]
            [advent-of-code-2024.utils :refer :all]))

(def input (-> "day-11.input"
               io/resource
               slurp
               s/trim
               (s/split #"\s+")
               (->> (mapv parse-long))))

(defn count-digits [n]
  (assert (pos? n))
  (-> n
      m/log10
      m/floor
      inc
      int))

(defn split-stone [stone]
  (let [n (count-digits stone)]
    (when (even? n)
      (let [m (m/pow 10 (/ n 2))]
        [(m/floor-div stone m)
         (m/floor-mod stone m)]))))

(defn change [stone]
  (or (and (zero? stone) [1])
      (split-stone stone)
      [(* stone 2024)]))

(defn blinks-of-one-stone [])

(defn -advance [mem n stones]
  (reduce
   (fn [[m t] s]
     (let [[mm v] (blinks-of-one-stone m n s)]
       [mm (+ t v)]))
   [mem 0]
   stones))

(defn blinks-of-one-stone [mem n stone]
  (if (zero? n)
    [mem 1]
    (if-let [v (mem [stone n])]
      [mem v]
      (let [[m v] (-advance mem (dec n) (change stone))]
        [(assoc m [stone n] v)
         v]))))

(defn count-after-blinks [n row]
  (->> row
       (-advance {} n)
       second))

(def solution1 (count-after-blinks 25 input))

(def solution2 #(count-after-blinks 75 input))
