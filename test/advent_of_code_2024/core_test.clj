(ns advent-of-code-2024.core-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.core :refer :all]))

(deftest a-test
  (testing "The right year"
    (is (= current-year 2024))))
