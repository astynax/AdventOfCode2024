(ns advent-of-code-2024.day-05
  (:require [clojure.java.io :as io]
            [clojure.math :refer [floor-div]]
            [advent-of-code-2024.utils :refer :all]))

(defn decode [raw]
  (let [lines (clojure.string/split-lines raw)
        raw-rules (take-while not-empty lines)
        raw-updates (rest (drop-while not-empty lines))
        rules (reduce (fn [m [k v]] (assoc m k (conj (get m k #{}) v)))
                      {}
                      (map #(map parse-long (clojure.string/split % #"\|"))
                           raw-rules))
        updates (map #(mapv parse-long (clojure.string/split % #","))
                     raw-updates)]
    {:rules rules
     :updates updates}))

(def input (decode (slurp (io/resource "day-05.input"))))

(def example (decode "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"))

(defn in-right-order? [rules update]
  (every?
   (fn [[x & xs]]
     (let [afters (get rules x #{})]
       (every? #(not (afters %)) xs))
     ;; (empty? (clojure.set/difference (get rules x) xs))
     )
   (tails (reverse update))))

(defn middle-of [v]
  (get v (floor-div (count v) 2)))

(defn sum-of-middles-of-rights [{:keys [rules updates]}]
  (->> updates
       (filter #(in-right-order? rules %))
       (map middle-of)
       (reduce +)))

(defn fix-order [rules pages]
  (vec
   (sort-by identity
            #(some? ((get rules %1 #{}) %2))
            pages)))

(defn sum-of-middles-of-fixed-ones [{:keys [rules updates]}]
  (->> updates
       (filter #(not (in-right-order? rules %)))
       (map #(fix-order rules %))
       (map middle-of)
       (reduce +)))

(def solution1 (sum-of-middles-of-rights input))

(def solution2 (sum-of-middles-of-fixed-ones input))
