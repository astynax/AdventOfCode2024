(ns advent-of-code-2024.day-02-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-02 :refer :all]))

(deftest safe?-test
  (is (safe? [1 2 3 4 5]))
  (is (safe? [5 4 3 2 1]))
  (is (safe? [0 2]))
  (is (safe? [0 3]))
  (is (safe? [2 0]))
  (is (safe? [3 0]))
  (is (not (safe? [0 0])))
  (is (not (safe? [0 4])))
  (is (not (safe? [4 0]))))

(deftest skip-test
  (is (= [] (skips [])))
  (is (= [[]] (skips [42])))
  (is (= [[2] [1]] (skips [1 2])))
  (is (= [[2 3 4]
          [1 3 4]
          [1 2 4]
          [1 2 3]] (skips [1 2 3 4]))))

(deftest tolerable-safe?-test
  (is (tolerable-safe? [1 2]))
  (is (tolerable-safe? [1 10]))
  (is (tolerable-safe? [1 5 2]))
  (is (tolerable-safe? [10 2 1])))

(deftest solution1-test
  (is (= 624 solution1)))

(deftest solution2-test
  (is (= 658 solution2)))
