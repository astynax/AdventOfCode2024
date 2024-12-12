(ns advent-of-code-2024.day-12-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-12 :refer :all]))

(deftest price-test
  (are [e p] (= p (first (prices e)))
    example1 140
    example2 772
    example3 1930)
  (is (= 1206 (second (prices example3)))))

(deftest scan-for-sides-test
  (let [region #{[0 0]       [2 0]
                 [0 1] [1 1] [2 1] [3 1]}]
    (are [s _ a1 a2 f1 f2] (= s (scan-for-sides region a1 a2 f1 f2))
      4 \v [0 1] [0 3] (fn [x y] [x y]) (fn [x y] [x (dec y)])
      1 \^ [1 0] [0 3] (fn [x y] [x y]) (fn [x y] [x (inc y)])
      2 \> [0 3] [0 1] (fn [y x] [x y]) (fn [y x] [(dec x) y])
      3 \< [3 0] [0 1] (fn [y x] [x y]) (fn [y x] [(inc x) y]))))

(deftest sides-test
  (is (= 10 (sides #{[0 0]       [2 0]
                     [0 1] [1 1] [2 1] [3 1]}))))

(deftest solution1-test
  (is (= 1319878 solution1)))

(deftest solution2-test
  (is (= 784982 solution2)))
