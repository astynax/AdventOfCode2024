(ns advent-of-code-2024.day-07
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :as m]))

(defn decode [text]
  (for [line (s/split-lines (s/trim text))
        :let [[v & ns] (s/split line #"\s+")]]
    [(parse-long (s/replace v ":" ""))
     (map parse-long ns)]))

(def input (decode (slurp (io/resource "day-07.input"))))

(def example (decode "
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
"))

(defn -check [ops goal total args]
  (cond
    (> total goal) false
    (empty? args) (= total goal)
    true (let [[a & as] args]
           (some? (some #(-check ops goal (% total a) as) ops)))))

(defn fixable? [ops [goal [a & args]]]
  (-check ops goal a args))

(defn total-calibration-result [ops eqs]
  (->> (for [e eqs
             :when (fixable? ops e)]
         (first e))
       (reduce +)))

(defn | [total suffix]
  (if (zero? suffix)
    (* total 10)
    (let [d (long (m/pow 10 (inc (m/floor (m/log10 suffix)))))]
      (+ (* total d) suffix))))

(def solution1 #(total-calibration-result [+ *] input))

(def solution2 #(total-calibration-result [+ * |] input))
