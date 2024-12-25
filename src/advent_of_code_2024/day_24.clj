(ns advent-of-code-2024.day-24
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [advent-of-code-2024.utils :refer :all]))

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

(defn number-from [prefix state]
  (->> state
       (filter #(s/starts-with? (first %) prefix))
       (sort-by first)
       (map second)
       reverse
       (reduce (fn [s b] (+ (* 2 s) (if b 1 0))) 0)))

#_ ;; this one generates a .dot file for the GraphViz
(with-open [f (clojure.java.io/writer "file:///tmp/adder.dot")]
  (binding [*out* f]
    (println "digraph adder {")
    (doseq [[op a b c] (:rules input)]
      (let [n (str "\"" (name op) "(" a ", " b ")\"")]
        (println a "->" n ";")
        (println b "->" n ";")
        (println n "->" c ";")))
    (println "}")))

;; I made this patch by looking with my own eyes at the graphvized plot :P
(def patch
  [[[:and "y07" "x07" "z07"] "gmt"]
   [[:xor "pmc" "mvw" "gmt"] "z07"]
   [[:xor "y11" "x11" "qjj"] "cbj"]
   [[:and "x11" "y11" "cbj"] "qjj"]
   [[:xor "hch" "nff" "dmn"] "z18"]
   [[:or  "khk" "stg" "z18"] "dmn"]
   [[:xor "qnm" "rfk" "cfk"] "z35"]
   [[:and "qnm" "rfk" "z35"] "cfk"]])

(def patched
  (let [m (->> (for [[[op a b _ :as k] v] patch]
                 [k [op a b v]])
               (into {}))]
    (update input :rules (partial replace m))))

(def solution1 (number-from "z" (evaluate input)))

(def solution2 (->> patch
                    (map second)
                    sort
                    (s/join ",")))
