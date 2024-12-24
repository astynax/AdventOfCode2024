(ns advent-of-code-2024.day-24
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn decode [text]
  (let [lines (s/split-lines (s/trim text))
        [inits [_ & raw-rules]] (split-with not-empty lines)
        state (->> (for [i inits
                         :let [[k v] (s/split i #"\s+")]]
                     [(s/replace k #":" "") (= "1" v)])
                   (into {}))
        rules (for [r raw-rules
                    :let [[a op b _ c] (s/split r #"\s+")]]
                [(keyword (s/lower-case op)) a b c])]
    {:state state
     :rules rules}))

(def input (decode (slurp (io/resource "day-24.input"))))

(def example (decode (slurp (io/resource "day-24-example.input"))))

(def minimal-example (decode "
b: 0
a: 1

c XOR d -> e
a OR b -> c
a AND b -> d
"))

(def ops {:and #(and %1 %2)
          :or #(or %1 %2)
          :xor (fn [a b] (or (and a (not b))
                            (and (not a) b)))})

(defn evaluate [{:keys [state rules]}]
  (loop [state state
         rules rules
         later ()]
    (cond
      (and (empty? rules) (empty? later))
      state

      (empty? rules)
      (recur state (reverse later) ())

      true
      (let [[[op ka kb kc :as r] & rs] rules
            a (state ka)
            b (state kb)]
        (if (and (some? a) (some? b))
          (recur (assoc state kc ((ops op) a b))
                 rs
                 later)
          (recur state
                 rs
                 (cons r later)))))))

(defn from-zs [state]
  (->> state
       (filter #(s/starts-with? (first %) "z"))
       (sort-by first)
       (map second)
       reverse
       (reduce (fn [s b] (+ (* 2 s) (if b 1 0))) 0)))

(def solution1 (from-zs (evaluate input)))

(def solution2 :TODO)
