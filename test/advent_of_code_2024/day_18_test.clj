(ns advent-of-code-2024.day-18-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-18 :refer :all]))

(deftest solution1-test
  (is (= 280 solution1)))

(deftest solution2-test
  #_ ;; quite long :P
  (is (= [28 56] solution2)))
