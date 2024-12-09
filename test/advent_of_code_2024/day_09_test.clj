(ns advent-of-code-2024.day-09-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-09 :refer :all]))

(deftest decode-test
  (is (= "00...111...2...333.44.5555.6666.777.888899"
         (dump (as-blocks example)))))

(deftest optimize-test
  (is (= "0099811188827773336446555566.............."
         (dump (optimize example)))))

(deftest insert-test
  (is (= [[1 0] [3 9] [1 nil] [2 1]]
         (insert [[1 0] [4 nil] [2 1]] [3 9])))
  (is (nil? (insert [[1 0] [4 nil] [2 1]] [5 9]))))

(deftest optimize-no-frag-test
  (is (= "00992111777.44.333....5555.6666.....8888.."
         (dump (optimize-no-frag example)))))

(deftest checksum-test
  (is (= 1928 (checksum (optimize example))))
  (is (= 2858 (checksum (optimize-no-frag example)))))

(deftest solution1-test
  (is (= 6201130364722 (solution1))))

#_ ; This one is lo-o-ong :P
(deftest solution2-test
  (is (= 6221662795602 (solution2))))
