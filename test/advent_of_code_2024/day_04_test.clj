(ns advent-of-code-2024.day-04-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-04 :refer :all]))

(deftest solution1-for-example
  (is (= 18 (count-all example))))

(deftest solution1-test
  (is (= 2603 solution1)))

(deftest solution2-for-example
  (is (= 9 (count-all-x-mas example))))

(deftest solution2-test
  (is (= 1965 solution2)))
