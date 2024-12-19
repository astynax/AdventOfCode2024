(ns advent-of-code-2024.day-19-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-19 :refer :all]))

(deftest solution1-test
  (is (= 342 solution1)))

(deftest solution2-test
  (is (= 891192814474630 solution2)))
