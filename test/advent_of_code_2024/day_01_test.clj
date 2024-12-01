(ns advent-of-code-2024.day-01-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-01 :refer :all]))

(deftest solution1-test
  (testing "First solution is right"
    (is (= solution1 2086478))))

(deftest solution2-test
  (testing "Second solution is right"
    (is (= solution2 24941624))))
