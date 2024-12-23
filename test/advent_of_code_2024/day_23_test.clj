(ns advent-of-code-2024.day-23-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-23 :refer :all]))

(deftest solution1-test
  (is (= 1075 solution1)))

(deftest solution2-test
  (is (= "az,cg,ei,hz,jc,km,kt,mv,sv,sx,wc,wq,xy" solution2)))
