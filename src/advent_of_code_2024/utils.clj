(ns advent-of-code-2024.utils
  (:require [clojure.string :as s]))

(defn tap [x]
  (println x)
  x)

(defn enumerate [xs]
  (map vector (iterate inc 0) xs))

(defn tails [xs] (take-while not-empty (iterate rest xs)))

(defn transpose [xss]
  (mapv (fn [i] (mapv #(get % i) xss))
        (range (count (first xss)))))

(defn keys-for [x m]
  (for [[k v] m :when (= v x)] k))

(defn topo-map
  ([text] (topo-map text (fn [_ v] v)))
  ([text look-at]
   (into
    {}
    (for [[y row] (enumerate (if (string? text)
                               (s/split-lines (s/trim text))
                               text))
          [x chr] (enumerate row)
          :let [pos [x y]
                v (look-at pos chr)]
          :when (some? v)]
      [pos v]))))

(defn topo-map-bound
  ([text] (topo-map-bound text (fn [_ v] v)))
  ([text look-at]
   (let [lines (s/split-lines (s/trim text))
         h (count lines)
         w (count (first lines))]
     {:map (topo-map lines look-at)
      :w w
      :h h
      :in-bounds? (fn [[x y]]
                    (and (<= 0 x) (< x w)
                         (<= 0 y) (< y h)))})))

(defn neibs [valid? [x y]]
  (->> [[(dec x) y]
        [(inc x) y]
        [x (dec y)]
        [x (inc y)]]
       (filter valid?)))

(defn manhattan [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1))
     (abs (- y2 y1))))

(def char->digit
  {\0 0
   \1 1
   \2 2
   \3 3
   \4 4
   \5 5
   \6 6
   \7 7
   \8 8
   \9 9})

(defn flatmap [f xs]
  (for [x xs
        r (f x)] r))

(defn stabilize [f x]
  (loop [x x
         p ::never]
    (let [nx (f x)]
      (if (= nx p)
        nx
        (recur nx x)))))
