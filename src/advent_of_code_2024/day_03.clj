(ns advent-of-code-2024.day-03
  (:require [clojure.java.io :as io]))

(def input (slurp (io/resource "day-03.input")))

(def example "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(def re-muls-only #"mul\((\d+),(\d+)\)")

(def example2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(def re-muls-and-conds #"(?:don't\(\))|(?:do\(\))|(?:mul\((\d+),(\d+)\))")

(defn parse-muls
  ([line]
   (parse-muls line re-muls-only))

  ([line pattern]
   (for [[m n1 n2] (re-seq pattern line)]
     (case m
       "don't()" false
       "do()" true
       [(parse-long n1) (parse-long n2)]))))

(defn evaluate [program]
  (loop [m 1
         steps program
         acc 0]
    (if (empty? steps) acc
        (let [[s & ss] steps]
          (case s
            true (recur 1 ss acc)
            false (recur 0 ss acc)
            (recur m ss (+ acc (apply * m s))))))))

(def solution1 (evaluate (parse-muls input)))

(def solution2 (evaluate (parse-muls input re-muls-and-conds)))
