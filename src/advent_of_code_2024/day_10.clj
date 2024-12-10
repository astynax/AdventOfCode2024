(ns advent-of-code-2024.day-10
  (:require [clojure.java.io :as io]
            [advent-of-code-2024.utils :refer :all]))

(defn decode [text]
  (topo-map text (fn [_ v] (char->digit v))))

(def input (decode (slurp (io/resource "day-10.input"))))

(def example (decode "
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
"))

(defn variants [area [x y]]
  (let [h (area [x y])]
    (if (= h 9)
      [[x y]]
      (->> [[(dec x) y]
            [(inc x) y]
            [x (dec y)]
            [x (inc y)]]
           (filter #(= h (dec (area % 0))))))))

(defn peaks-from [area start]
  (->> [start]
       (stabilize (partial flatmap (partial variants area)))
       (into #{})))

(defn all-scores [area]
  (->> (keys-for 0 area)
       (map #(count (peaks-from area %)))
       (reduce +)))

(defn trails-from [area start]
  (->> [[start]]
       (stabilize
        (partial flatmap (fn [[x :as path]]
                           (if (= 9 (area x))
                             [path]
                             (for [v (variants area x)]
                               (cons v path))))))
       (into #{})))

(defn all-ratings [area]
  (->> (keys-for 0 area)
       (map #(count (trails-from area %)))
       (reduce +)))

(def solution1 (all-scores input))

(def solution2 (all-ratings input))
