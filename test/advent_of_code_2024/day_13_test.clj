(ns advent-of-code-2024.day-13-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-13 :refer :all]))

(deftest naive-score-test
  (is (= 480 (score example))))

(deftest solution1-test
  (is (= 27105 solution1)))

(deftest solution2-test
  (is (= 101726882250942 solution2)))
