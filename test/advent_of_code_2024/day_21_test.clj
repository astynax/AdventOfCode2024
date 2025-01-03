(ns advent-of-code-2024.day-21-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-21 :refer :all]))

(deftest solve-test
  (is (= 126384 (solve 2 (decode "
029A
980A
179A
456A
379A
")))))

(deftest solution1-test
  (is (= 188384 solution1)))

(deftest solution2-test
  (is (= :TODO solution2)))
