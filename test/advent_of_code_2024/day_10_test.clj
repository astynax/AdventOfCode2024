(ns advent-of-code-2024.day-10-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-10 :refer :all]))

(deftest all-scores-test
  (is (= 36 (all-scores example))))

(deftest all-ratings-test
  (is (= 81 (all-ratings example))))

(deftest solution1-test
  (is (= 811 solution1)))

(deftest solution2-test
  (is (= 1794 solution2)))
