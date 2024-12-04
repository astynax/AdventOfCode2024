(ns advent-of-code-2024.utils)

(defn enumerate [xs]
  (map vector (iterate inc 0) xs))

(defn tails [xs] (take-while not-empty (iterate rest xs)))

(defn transpose [xss]
  (mapv (fn [i] (mapv #(get % i) xss))
        (range (count (first xss)))))
