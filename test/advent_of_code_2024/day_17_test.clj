(ns advent-of-code-2024.day-17-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-17 :refer :all]))

(deftest solution1-test
  (is (= "6,1,6,4,2,4,7,3,5" solution1)))

(deftest solution2-test
  (is (= 202975183645226 solution2)))
