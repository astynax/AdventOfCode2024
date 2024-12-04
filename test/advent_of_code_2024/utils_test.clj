(ns advent-of-code-2024.utils-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.utils :refer :all]))

(deftest enumerate-test
  (is (= '() (enumerate "")))
  (is (= '([0 \b] [1 \a] [2 \r])
         (enumerate "bar"))))

(deftest transpose-test
  (is (= [] (transpose [[]]))) ;; TODO: should be [[]]
  (is (= [[0]] (transpose [[0]])))
  (is (= [[1] [2]] (transpose [[1 2]])))
  (is (= [[1 4]
          [2 5]
          [3 6]]
         (transpose [[1 2 3]
                     [4 5 6]]))))
