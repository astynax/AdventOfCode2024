(ns advent-of-code-2024.day-08
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [advent-of-code-2024.utils :refer :all]))

(defn decode [text]
  (topo-map-bound text (fn [_ v] (when-not (= v \.) v))))

(def input (decode (slurp (io/resource "day-08.input"))))

(def example (decode "
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
"))

(defn next-pos [[x0 y0] [x1 y1]]
  [(+ x1 (- x1 x0))
   (+ y1 (- y1 y0))])

(defn nearest [p1 p2 in-bounds?]
  (filter in-bounds?
          [(next-pos p1 p2) (next-pos p2 p1)]))

(defn trace [f t]
  (->> [f t]
       (iterate (fn [[f t]] [t (next-pos f t)]))
       (map second)
       rest))

(defn harmonics [p1 p2 in-bounds?]
  (concat (take-while in-bounds? (trace p1 p2))
          (take-while in-bounds? (trace p2 p1))))

(defn antinodes [{:keys [in-bounds?]
                  m :map}
                 detect]
  (into
   #{}
   (for [[p1 a1] m
         [p2 a2] m
         :when (and (= a1 a2)
                    (not= p1 p2))
         an (detect p1 p2 in-bounds?)]
     an)))

(defn count-harmonics-and-antennas [area]
  (count
   (set/union
    (antinodes area harmonics)
    (keys (:map area)))))

(def solution1 (count (antinodes input nearest)))

(def solution2 (count-harmonics-and-antennas input))
