(ns advent-of-code-2024.day-07-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-07 :refer :all]))

(deftest fixable?-test
  (is (fixable? [+ *] [190 '(10 19)]))
  (is (fixable? [+ *] [3267 '(81 40 27)]))
  (is (fixable? [+ *] [292 '(11 6 16 20)]))
  (is (not (fixable? [+ *] [7290 '(6 8 6 15)])))
  (is (fixable? [+ * |] [7290 '(6 8 6 15)]))
  (is (fixable? [+ * |] [156 '(15 6)]))
  (is (fixable? [+ * |] [192, '(17 8 14)])))

(deftest total-calibration-result-test
  (is (= 3749 (total-calibration-result [+ *] example)))
  (is (= 11387 (total-calibration-result [+ * |] example))))

(deftest |-test
  (is (= 1234 (| 123 4)))
  (is (= 1234 (| 12 34)))
  (is (= 1234 (| 1 234)))
  (is (= 1026766857276279 (| 102676685 7276279)))
  (testing "tricky cases"
    (is (= 100 (| 10 0)))
    (is (= 101 (| 10 1)))
    (is (= 1010 (| 10 10)))))

(deftest solution1-test
  (is (= 42283209483350 (solution1))))

(deftest solution2-test
  (is (= 1026766857276279 (solution2))))
