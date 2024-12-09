(ns advent-of-code-2024.day-04
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [advent-of-code-2024.utils :refer [enumerate keys-of]]))

(defn decode [raw]
  (reduce
   (fn [m [k v]] (assoc m k v)) {}
   (for [[y line] (-> raw
                      s/trim
                      s/split-lines
                      enumerate)
         [x c] (enumerate line)]
     [[x y] c])))

(def input (decode (slurp (io/resource "day-04.input"))))

(def example (decode "
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
"))

(defn gather [m [x0 y0] [dx dy]]
  (for [i (range 4)
        :let [x (+ x0 (* dx i))
              y (+ y0 (* dy i))]]
    (get m [x y])))

(def steps
  [[-1 -1] [0 -1] [1 -1]
   [-1  0]        [1  0]
   [-1  1] [0  1] [1  1]])

(defn count-at [m pos]
  (->> steps
       (map #(gather m pos %))
       (filter #(= % '(\X \M \A \S)))
       count))

(defn count-all [m]
  (->> m
       (keys-for \X)
       (map #(count-at m %))
       (reduce +)))

(defn x-mas-at? [m [x y]]
  (and
   (= \A (m [x y]))
   (let [x-es [(m [(dec x) (dec y)])
               (m [(dec x) (inc y)])
               (m [(inc x) (dec y)])
               (m [(inc x) (inc y)])]]
     (or (= x-es [\M \M \S \S])
         (= x-es [\M \S \M \S])
         (= x-es [\S \S \M \M])
         (= x-es [\S \M \S \M])))))

(defn count-all-x-mas [m]
  (->> (keys-for \A m)
       (filter #(x-mas-at? m %))
       count))

(def solution1 (count-all input))

(def solution2 (count-all-x-mas input))
