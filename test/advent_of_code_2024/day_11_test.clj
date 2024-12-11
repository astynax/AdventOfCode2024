(ns advent-of-code-2024.day-11-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-11 :refer :all]))

(deftest split-stone-test
  (are [s e] (= e (split-stone s))
    1234   [12 34]
    12     [1 2]
    123    nil
    123456 [123 456]
    123000 [123 0]))

(deftest count-after-blinks-test
  (is (= 55312 (blinks-of-many 25 [125 17]))))

(deftest solution1-test
  (is (= 186424 solution1)))

(deftest solution2-test
  (is (= 219838428124832 solution2)))
