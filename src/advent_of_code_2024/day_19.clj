(ns advent-of-code-2024.day-19
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn decode [text]
  (let [[ts _ & ps] (s/split-lines (s/trim text))
        ts (s/split ts #",\s+")]
    {:ts ts
     :ps ps}))

(def input (decode (slurp (io/resource "day-19.input"))))

(def example (decode "
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
"))

(defn fork [ts p]
  (for [t ts
        :when (s/starts-with? p t)]
    [t p]))

(defn possible? [ts pattern]
  (loop [n 0
         queue (fork ts pattern)]
    (if (empty? queue)
      false
      (let [[[t p] & queue] queue]
        (if (= t p)
          true
          (->> (subs p (count t))
               (fork ts)
               (into queue)
               (recur n)))))))

(defn count-possible [{:keys [ts ps]}]
  (->> ps
       (filter #(possible? ts %))
       count))

(defn count-combinations [ts pattern]
  (let [n (count pattern)
        dp (vec (cons 1 (repeat n 0)))]
    (loop [i 1
           dp dp]
      (if (> i n)
        (dp n)
        (->> (reduce
              (fn [dp t]
                (let [l (count t)]
                  (if (and (>= i l)
                           (= t (subs pattern (- i l) i)))
                    (update dp i + (dp (- i l)))
                    dp)))
              dp
              ts)
             (recur (inc i)))))))

(defn count-all-combinations [{:keys [ts ps]}]
  (->> ps
       (map #(count-combinations ts %))
       (reduce +)))

(def solution1 (count-possible input))

(def solution2 (count-all-combinations input))
