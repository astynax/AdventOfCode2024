(ns advent-of-code-2024.day-06
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [advent-of-code-2024.utils :refer :all]))

(defn decode [text]
  (let [m (topo-map text)
        start (some (fn [[k v]] (when (= v \^) k)) m)]
    {:area (assoc m start \.)
     :start start
     :dir :u}))

(def input (decode (slurp (io/resource "day-06.input"))))

(def example (decode "

....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...

"))

(defn turn-right [d]
  (case d
    :u :r
    :r :d
    :d :l
    :l :u))

(defn advance [[x y] d]
  (case d
    :u [x (dec y)]
    :d [x (inc y)]
    :l [(dec x) y]
    :r [(inc x) y]))

(defn walk [{area :area :as state}]
  (loop [pos (:start state)
         dir (:dir state)
         visited '()]
    (let [new-pos (advance pos dir)
          visited (conj visited pos)]
      (case (area new-pos)
        \. (recur new-pos dir visited)
        \# (recur pos (turn-right dir) visited)
        visited))))

(defn walk-and-count [state]
  (count (apply hash-set (walk state))))

(defn traverse [& {:keys [area start extra on-step]}]
  (loop [extra extra
         {:keys [pos dir cached]} start]
    (let [new-pos (advance pos dir)]
      (if (cached [new-pos dir])
        {:looped true
         :extra extra}
        (case (area new-pos)
          \. (recur (on-step :area area
                             :pos pos
                             :new-pos new-pos
                             :dir dir
                             :cached cached
                             :extra extra)
                    {:pos new-pos
                     :dir dir
                     :cached (conj cached [pos dir])})

          \# (recur extra
                    {:pos pos
                     :dir (turn-right dir)
                     :cached (conj cached [pos dir])})

          {:looped false
           :extra extra})))))

(defn walk-and-find-cycles [config]
  (->
   (traverse
    :area (:area config)
    :start {:pos (:start config)
            :dir (:dir config)
            :cached #{}}
    :extra {:visited #{}
            :stops #{}}
    :on-step
    (fn [& {:keys [area pos new-pos dir cached]
           {:keys [stops visited]} :extra}]
      (let [looped
            (and
             (not (visited new-pos))
             (:looped
              (traverse
               :area (assoc area new-pos \#)
               :start {:pos pos
                       :dir dir
                       :cached cached}
               :extra nil
               :on-step (fn [& _] nil))))]
        {:visited (conj visited new-pos)
         :stops (if looped
                  (conj stops new-pos)
                  stops)})))
   :extra
   :stops))

(defn count-exploitable-places [state]
  (->> state
       walk-and-find-cycles
       count))

(def solution1 (walk-and-count input))

(def solution2 #(count-exploitable-places input))
