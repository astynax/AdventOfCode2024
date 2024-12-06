(ns advent-of-code-2024.utils
  (:require [clojure.string :as s]))

(defn enumerate [xs]
  (map vector (iterate inc 0) xs))

(defn tails [xs] (take-while not-empty (iterate rest xs)))

(defn transpose [xss]
  (mapv (fn [i] (mapv #(get % i) xss))
        (range (count (first xss)))))

(defn topo-map
  ([text] (topo-map text (fn [_ v] v)))
  ([text look-at]
   (reduce
    (fn [m [k v]] (assoc m k v))
    {}
    (for [[y row] (enumerate (s/split-lines (s/trim text)))
          [x chr] (enumerate row)
          :let [pos [x y]
                v (look-at pos chr)]
          :when (some? v)]
      [pos v]))))
