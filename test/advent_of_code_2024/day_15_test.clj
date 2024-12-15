(ns advent-of-code-2024.day-15-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-15 :refer :all]))

(deftest all-gps-for-test
  (are [g e] (= g (->> (simulate-using step e)
                       :m
                       (all-gps-for \O)))
    10092 example
    2028 example2)
  (is (= 9021 (->> example
                   widen
                   (simulate-using wide-step)
                   :m
                   (all-gps-for \[)))))

(deftest solution1-test
  (is (= 1514353 solution1)))

(deftest solution2-test
  (is (= 1533076 solution2)))
