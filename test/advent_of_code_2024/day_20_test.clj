(ns advent-of-code-2024.day-20-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-20 :refer :all]))

(deftest trace-test
  (is (= 44 (trace example 2 2)))
  (is (= 285 (trace example 50 20))))

(deftest solution1-test
  (is (= 1395 solution1)))

(deftest solution2-test
  (is (= 993178 solution2)))
