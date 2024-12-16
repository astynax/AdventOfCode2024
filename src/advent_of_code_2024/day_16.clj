(ns advent-of-code-2024.day-16
  (:require [clojure.java.io :as io]
            [clojure.data.priority-map :refer [priority-map]]
            [loom.graph :refer [fly-graph]]
            [loom.alg :refer [dijkstra-path-dist]]
            [advent-of-code-2024.utils :refer :all]))

(defn decode [text]
  (let [{m :map
         :keys [w h]} (topo-map-bound text (fn [_ c] (when (= c \.) c)))
        ps [1 (- h 2)]
        pg [(- w 2) 1]]
    {:m (conj (into #{} (keys m)) ps pg)
     :start ps
     :goal pg}))

(def input (decode (slurp (io/resource "day-16.input"))))

(def example (decode "
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
"))

(def turn-left {\^ \<
                \< \v
                \v \>
                \> \^})

(def turn-right {\^ \>
                 \> \v
                 \v \<
                 \< \^})

(defn move [[x y] d]
  (case d
    \^ [x (dec y)]
    \v [x (inc y)]
    \< [(dec x) y]
    \> [(inc x) y]))

(defn successors [m [p d]]
  (let [np (move p d)
        vs (list [p (turn-left d)]
                 [p (turn-right d)])]
    (if (contains? m np)
      (cons [np d] vs)
      vs)))

(defn weight [[_ d1] [_ d2]]
  (if (= d1 d2) 1 1000))

(defn ->fdg [m]
  (fly-graph :successors (partial successors m)
             :weight weight))

(defn score [{:keys [m start goal]}]
  (let [[p s] (dijkstra-path-dist (->fdg m) [start \>] [goal \>])
        [[_ d1] [_ d2] & _] (reverse p)]
    (if (= d1 d2) s (- s 1000))))

(defn seats [{:keys [m start goal]}]
  (loop [vs {}
         bs #{}
         queue (priority-map [[start \>]] 0)
         min-score java.lang.Long/MAX_VALUE]
    (if-not (seq queue)
      (count bs)
      (let [[path score] (peek queue)
            p (peek path)]
        (if (= goal (first p))
          (if (<= score min-score)
            (recur vs
                   (into bs (map first path))
                   (pop queue)
                   score)
            (count bs))
          (let [queue (reduce (fn [q target]
                                (let [s (+ score (weight p target))]
                                  (if (< (vs target Long/MAX_VALUE) s)
                                    q
                                    (assoc q (conj path target) s))))
                              (pop queue)
                              (successors m p))]
            (recur (assoc vs p score)
                   bs queue min-score)))))))

(def solution1 (score input))

(def solution2 #(seats input))
