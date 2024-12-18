(ns advent-of-code-2024.day-18
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [loom.alg :refer [bf-path dijkstra-path]]
            [loom.graph :refer [fly-graph]]))

(defn decode [text]
  (for [line (s/split-lines (s/trim text))
        :let [[n1 n2] (s/split line #",")]]
    [(parse-long n1) (parse-long n2)]))

(def input {:mx 70
            :my 70
            :bs (decode (slurp (io/resource "day-18.input")))})

(def example {:mx 6
              :my 6
              :bs (list [5 4] [4 2] [4 5] [3 0] [2 1] [6 3] [2 4]
                        [1 5] [0 6] [3 3] [2 6] [5 1] [1 2] [5 5]
                        [2 5] [6 5] [1 4] [0 4] [6 4] [1 1]
                        [6 1] [1 0] [0 5] [1 6] [2 0])})

(defn ->fg [{:keys [mx my bs]} n]
  (let [blocks (set (take n bs))]
    (fly-graph
     :successors
     (fn [[x y]]
       (for [[x y :as pos] [[x (dec y)]
                            [x (inc y)]
                            [(dec x) y]
                            [(inc x) y]]
             :when (and (<= 0 x mx)
                        (<= 0 y my)
                        (not (blocks pos)))]
         pos)))))

(defn simulate [n {:keys [mx my] :as data}]
  (-> (->fg data n)
      (bf-path [0 0] [mx my])
      count
      dec))

(defn first-blocker-after [n data]
  (let [l (count (:bs data))]
    (loop [i n]
      (when (< i l)
        (if (= -1 (simulate i data))
          (nth (:bs data) (dec i))
          (recur (inc i)))))))

(def solution1 (simulate 1024 input))

(def solution2 (first-blocker-after 1024 input))
