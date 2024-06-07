(ns clojureuler.problem6-test
  (:require [clojure.test :refer :all])
  (:require [clojureuler.problem6 :refer [sum-of-squares square-of-sums]]))

(deftest sum-of-squares-test
  (testing "Sum of squares"
    (is (= (sum-of-squares 10) 385))))

(deftest square-of-sums-test
  (testing "Square of sums"
    (is (= (square-of-sums 10) 3025))))
