(ns advent-of-code-2024.day-03-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-03 :refer :all]))

(deftest parse-muls-test
  (is (= [[2 4]
          [5 5]
          [11 8]
          [8 5]]
         (parse-muls example)))
  (is (= [[2 4]
          false
          [5 5]
          [11 8]
          true
          [8 5]]
         (parse-muls example2 re-muls-and-conds))))

(deftest evaluate-test
  (is (= 10
         (evaluate [[2 3]
                    false
                    [10 20]
                    true
                    [2 2]]))))

(deftest solution1-test
  (is (= 192767529 solution1)))

(deftest solution2-test
  (is (= 104083373 solution2)))
