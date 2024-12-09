(ns advent-of-code-2024.utils
  (:require [clojure.string :as s]))

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
