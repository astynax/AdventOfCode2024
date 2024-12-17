(ns advent-of-code-2024.day-17
  (:require [clojure.java.io :as io]
            [clojure.math :as m]
            [clojure.string :as s]))

(defn decode [text]
  (let [[a b c _ p] (s/split-lines (s/trim text))
        [a b c] (map #(parse-long (subs % 12)) [a b c])
        pgm (-> p
                (subs 9)
                (s/split #",")
                (->> (map parse-long))
                vec)]
    {:a a
     :b b
     :c c
     :pgm pgm
     :pc 0
     :out []}))

(def input (decode (slurp (io/resource "day-17.input"))))

(def example (decode "
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
"))

(defn arg-> [state arg]
  (case arg
    4 (:a state)
    5 (:b state)
    6 (:c state)
    arg))

(defn fwd [state]
  (update state :pc + 2))

(defn div-to [k {:keys [a] :as state} arg]
  (->> (arg-> state arg)
       (m/pow 2)
       (m/floor-div a)
       (assoc state k)))

(defn step [{:keys [pgm pc] :as state}]
  (when (< pc (dec (count pgm)))
    (let [[opcode arg] (drop pc pgm)]
      (case opcode
        ;; adv
        0 (fwd (div-to :a state arg))
        ;; bxl
        1 (fwd (assoc state :b (bit-xor (:b state) arg)))
        ;; bst
        2 (fwd (assoc state :b (mod (arg-> state arg) 8)))
        ;; jnz
        3 (if (zero? (:a state))
            (fwd state)
            (assoc state :pc arg))
        ;; bxc
        4 (fwd (assoc state :b (bit-xor (:b state) (:c state))))
        ;; out
        5 (fwd (update state :out conj (mod (arg-> state arg) 8)))
        ;; bdv
        6 (fwd (div-to :b state arg))
        ;; cdv
        7 (fwd (div-to :c state arg))
        ))))

(defn run [state]
  (loop [state state]
    (if-let [ns (step state)]
      (recur ns)
      (:out state))))

(defn output-after [state]
  (->> (run state)
       (s/join ",")))

(defn deduct-a [pgm]
  (loop [[[a pos] & stack] (list [0 (dec (count pgm))])]
    (if (< pos 0) a
        (->> (for [i (range 0 8)
                   :let [x (+ i (* a 8))
                         out (run {:a x :b 0 :c 0
                                   :pgm pgm :pc 0
                                   :out []})]
                   :when (= (drop pos pgm) out)]
               [x (dec pos)])
             reverse
             (into stack)
             recur))))

(def solution1 (output-after input))

(def solution2 (deduct-a (:pgm input)))
