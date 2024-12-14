(ns advent-of-code-2024.day-14-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-14 :refer :all]))

(deftest solution1-test
  (is (= 229868730 solution1)))

(deftest solution2-test
  (is (= 7861 solution2)))
