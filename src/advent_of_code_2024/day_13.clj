(ns advent-of-code-2024.day-13
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :as m]))

(def behaviour #"Button A: X\+(?<ax>\d+), Y\+(?<ay>\d+)
Button B: X\+(?<bx>\d+), Y\+(?<by>\d+)
Prize: X=(?<px>\d+), Y=(?<py>\d+)")

(defn decode [text]
  (for [[_ & ds] (re-seq behaviour text)
        :let [[ax ay bx by px py] (map parse-long ds)]]
    {:a [ax ay]
     :b [bx by]
     :p [px py]}))

(def input (decode (slurp (io/resource "day-13.input"))))

(def example (decode "
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
"))

(defn patch [game]
  (-> game
      (update-in [:p 0] + 10000000000000)
      (update-in [:p 1] + 10000000000000)))

(defn det [[a1 b1] [a2 b2]]
  (- (* a1 b2) (* a2 b1)))

(defn solve-le [[a1 b1 c1]
                [a2 b2 c2]]
  (let [d (det [a1 b1]
               [a2 b2])
        da (det [c1 b1]
                [c2 b2])
        db (det [a1 c1]
                [a2 c2])]
    [(/ da d)
     (/ db d)]))

(defn play [{[ax ay :as a] :a
             [bx by :as b] :b
             [px py :as p] :p}]
  (let [[a b] (solve-le [ax bx px]
                        [ay by py])]
    (when (not (or (ratio? a) (ratio? b)))
      (+ (* 3 a) b))))

(defn score [ games]
  (->> games
       (map play)
       (filter some?)
       (reduce +)))

(def solution1 (score input))

(def solution2 (score (map patch input)))
