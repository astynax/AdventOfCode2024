(ns advent-of-code-2024.day-14
  (:require [clojure.java.io :as io]
            [clojure.math :as m]))

(def pattern #"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)")

(defn decode [text]
  (for [[_ & ns] (re-seq pattern text)
        :let [[x y dx dy] (map parse-long ns)]]
    {:x x
     :y y
     :dx dx
     :dy dy}))

(def input {:rs (vec (decode (slurp (io/resource "day-14.input"))))
            :w 101
            :h 103})

(def example {:w 11
              :h 7
              :rs (decode "
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
")})

(defn step [{:keys [rs w h] :as area}]
  (->> (for [{:keys [x y dx dy] :as r} rs]
         (assoc r
                :x (mod (+ x dx) w)
                :y (mod (+ y dy) h)))
       vec
       (assoc area :rs)))

(defn steps [n area]
  (if (pos? n)
    (recur (dec n) (step area))
    area))

(defn safety [{:keys [rs w h]}]
  (let [hw (m/floor-div w 2)
        hh (m/floor-div h 2)]
    (->> (for [[x0 x1] [[0 hw] [(- w hw) w]]
               [y0 y1] [[0 hh] [(- h hh) h]]]
           (->> (for [{:keys [x y]} rs
                      :when (and (<= x0 x) (< x x1)
                                 (<= y0 y) (< y y1))]
                  1)
                (reduce +)))
         (reduce *))))

(defn dump [{:keys [rs w h]}]
  (doseq [r (range h)]
    (doseq [c (range w)]
      (let [v (->> (for [{:keys [x y]} rs
                         :when (and (= x c) (= y r))]
                     1)
                   (reduce +))]
        (print (if (zero? v) "." (str v)))))
    (println)))

(defn animate [area]
  (loop [area (steps 33 area)
         i 0]
    (dump area)
    (println i)
    (if (empty? (read-line))
      (recur (steps 103 area) (inc i))
      i)))

(def solution1 (safety (steps 100 input)))

(def solution2 (+ 33 (* 76 103)))
#_(dump (steps (+ 33 (* 76 103)) input))
