(ns advent-of-code-2024.day-24-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.day-24 :refer :all]))

(deftest decode-test
  (is (= {:state {"a" true "b" false}
          :rules '([:xor "c" "d" "e"]
                   [:or "a" "b" "c"]
                   [:and "a" "b" "d"])}
         minimal-example)))

(deftest evaluate-test
  (is (= {"a" true
          "b" false
          "c" true
          "d" false
          "e" true}
         (evaluate minimal-example))))

(deftest fromzs-test
  (is (= 5 (from-zs {"a" false "b" true
                     "z1" false "z2" true "z0" true}))))

(deftest solution1-test
  (is (= 59364044286798 solution1)))

(deftest solution2-test
  (is (= :TODO solution2)))
