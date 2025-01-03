(ns advent-of-code-2024.day-21
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [loom.graph :as g]
            [loom.alg :as a]
            [loom.label :as l]
            [advent-of-code-2024.utils :refer :all]))

(defn decode [text]
  (->> text
       s/trim
       s/split-lines))

(def input (decode (slurp (io/resource "day-21.input"))))

(defn label-for [[x1 y1] [x2 y2]]
  (case [(- x2 x1) (- y2 y1)]
    [1  0] \>
    [-1 0] \<
    [0  1] \v
    [0 -1] \^
    nil))

(defn ->keypad [text]
  (let [m (topo-map text (fn [_ c] (when (not= c \.) c)))]
    (->> (for [[k1 v1] m
               [k2 v2] m
               :when (not= k1 k2)
               :let [lbl (label-for k1 k2)]
               :when (some? lbl)]
           [v1 v2 lbl])
         (reduce (fn [g [n1 n2 lbl]]
                   (l/add-labeled-edges g [n1 n2] lbl))
                 (g/digraph)))))

(def numpad
  "A graph representing the numeric keypad
  +---+---+---+
  | 7 | 8 | 9 |
  +---+---+---+
  | 4 | 5 | 6 |
  +---+---+---+
  | 1 | 2 | 3 |
  +---+---+---+
      | 0 | A |
      +---+---+"
  (->keypad "
789
456
123
.0A
"))

(def d-pad
  "A graph that represents the directional keypad
      +---+---+
      | ^ | A |
  +---+---+---+
  | < | v | > |
  +---+---+---+"
  (->keypad "
.^A
<v>
"))

(defn path-quality [p]
  (let [l (->> (partition 2 1 p)
               (map (partial apply not=))
               (map #(if % 1 0))
               (reduce +))
        s (->> p
               (map {\< 1
                     \^ 3
                     \v 2
                     \> 3})
               (map * (iterate dec -1))
               (reduce +))]
    [l s]))

(defn path [g s e]
  (let [ps (loop [queue (list (list s))
                  seen #{s}]
             (let [done (filter #(= e (first %)) queue)]
               (if (seq done)
                 done
                 (let [q (for [[h :as ps] queue
                               n (g/successors g h)
                               :when (not (seen n))]
                           (cons n ps))]
                   (recur q (->> q
                                 (map first)
                                 (into seen)))))))
        ps (for [p ps]
             (for [[a b] (partition 2 1 (reverse p))]
               (l/label g a b)))]
    (->> ps
         (sort-by path-quality)
         first)))

(defn all-paths [g]
  (->> (for [n1 (g/nodes g)
             n2 (g/nodes g)]
         [[n1 n2] (str (s/join (path g n1 n2)) "A")])
       (into {})))

(def numpad-paths (all-paths numpad))

(def d-pad-paths (all-paths d-pad))

(defn trace [ps steps]
  (->> (cons \A steps)
       (partitionv 2 1)
       (map ps)))

(def trace-through-d-pads
  (memoize
   (fn [n steps]
     (if (zero? n)
       (count steps)
       (->> (trace d-pad-paths steps)
            ;; пробовать все пути и выбирать минимальный
            (map #(trace-through-d-pads (dec n) %))
            (reduce +))))))

(def path-through-d-pads
  (fn [n steps]
    (if (zero? n)
      steps
      (->> (trace d-pad-paths steps)
           (map #(path-through-d-pads (dec n) %))
           s/join))))

(defn trace-through [n num]
  (->> num
       (trace numpad-paths)
       s/join
       (trace-through-d-pads n)))

(defn solve [n nums]
  (->> (for [num nums
             :let [p (trace-through n num)
                   v (parse-long (s/replace num #"A" ""))]]
         (* p v))
       (reduce +)))

(comment
  (println)
  (println (path-through-d-pads 2 "<A^A>^^AvvvA")) ;; WHY?
  (println "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A"))

(def solution1 (solve 2 input))

;; 232389969568832 !!!
(def solution2 :TODO #_(solve 25 input))
