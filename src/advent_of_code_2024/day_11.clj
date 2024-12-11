(ns advent-of-code-2024.day-11
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :as m]))

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

(defn change-stone [stone]
  (or (and (zero? stone) [1])
      (split-stone stone)
      [(* stone 2024)]))

(def blinks-of-one-stone :later)

(defn blinks-of-many [n stones]
  (->> stones
       (map (partial blinks-of-one-stone n))
       (reduce +)))

(def blinks-of-one-stone
  (memoize
   (fn [n stone]
     (if (zero? n) 1
         (blinks-of-many (dec n) (change-stone stone))))))

(def solution1 (blinks-of-many 25 input))

(def solution2 (blinks-of-many 75 input))
