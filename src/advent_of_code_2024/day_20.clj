(ns advent-of-code-2024.day-20
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [loom.alg-generic :as a]
            [loom.graph :refer [fly-graph]]
            [advent-of-code-2024.utils :refer :all]))

(defn decode [text]
  (let [m (topo-map (s/trim text) (fn [_ c] (when (not= c \#) c)))
        [s] (keys-for \S m)
        [e] (keys-for \E m)]
    {:m (-> (set (keys m))
            (conj s e))
     :s s
     :e e}))

(def input (decode (slurp (io/resource "day-20.input"))))

(def example (decode "
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
"))

(defn trace [{:keys [m s e]} thr cheat]
  (->> (a/dijkstra-path #(neibs m %) (fn [_ _] 1) s e)
       enumerate
       tails
       (map (fn [[[i p] & rs]]
              (->> rs
                   (drop thr)
                   (filter (fn [[j r]]
                             (let [jump (manhattan r p)
                                   win (- j i)]
                               (and (<= jump cheat)
                                    (>= (- win jump) thr)))))
                   count)))
       (reduce +)))

(def solution1 (trace input 100 2))

(def solution2 (trace input 100 20))
