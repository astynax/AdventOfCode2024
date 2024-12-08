(ns advent-of-code-2024.day-08-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-08 :refer :all]))

(deftest antinodes-test
  (is (= 14 (count (antinodes example nearest))))
  (is (= 29 (count (antinodes example harmonics)))))

(deftest count-harmonics-and-antennas-test
  (is (= 34 (count-harmonics-and-antennas example))))

(deftest solution1-test
  (is (= 361 solution1)))

(deftest solution2-test
  (is (= 1249 solution2)))
