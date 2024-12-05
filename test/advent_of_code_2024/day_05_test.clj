(ns advent-of-code-2024.day-05-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-05 :refer :all]))

(deftest in-right-order?-test
  (is (= '(true
           true
           true
           false
           false
           false)
         (map #(in-right-order? (:rules example) %)
              (:updates example)))))

(deftest middle-of-test
  (is (= 3 (middle-of [1 2 3 4 5])))
  (is (= 2 (middle-of [1 2 3]))))

(deftest sum-of-middles-of-rights-test
  (is (= 143 (sum-of-middles-of-rights example))))

(deftest fix-order-test
  (are [u _ e] (= e (fix-order (:rules example) u))
    [75 97 47 61 53] :becomes [97 75 47 61 53]
    [61 13 29] :becomes [61 29 13]
    [97 13 75 29 47] :becomes [97 75 47 29 13]))

(deftest sum-of-middles-of-fixed-ones-test
  (is (= 123 (sum-of-middles-of-fixed-ones example))))

(deftest solution1-test
  (is (= 5991 solution1)))

(deftest solution2-test
  (is (= 5479 solution2)))
