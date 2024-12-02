(ns advent-of-code-2024.day-01-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-01 :refer :all]))

(deftest solution1-test
  (is (= 2086478 solution1)))

(deftest solution2-test
  (is (= 24941624 solution2)))
