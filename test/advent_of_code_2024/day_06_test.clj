(ns advent-of-code-2024.day-06-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-06 :refer :all]))

(deftest walk-and-count-test
  (is (= 41 (walk-and-count example))))

(deftest walk-and-find-cycles-test
  (is (= #{[0 3]}
         (walk-and-find-cycles (decode "

.#...
....#
.....
.....
.^.#.

"))))

  (is (= #{} (walk-and-find-cycles (decode "

.#.
#..
...
.^.

"))))

  (is (= #{[7 9] [7 7] [1 8] [3 8] [6 7] [3 6]}
         (walk-and-find-cycles example))))

(deftest count-exploitable-places-test
  (is (= 6 (count-exploitable-places example))))

(deftest solution1-test
  (is (= 4656 solution1)))

(deftest solution2-test
  (is (= 1575 (solution2))))
