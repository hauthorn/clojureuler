(ns clojureuler.problem4-test
  (:require [clojure.test :refer :all]
            [clojureuler.problem4 :refer [num-to-seq palindrome?]]))

(deftest num-to-seq-test
  (testing "Number to sequence"
    (is (= (num-to-seq 9009) [9 0 0 9]))
    (is (= (num-to-seq 9) [9]))
    (is (= (num-to-seq 124) [1 2 4]))
    ))

(deftest palindrome?-test
  (testing "Palindrome? False"
    (is (= (palindrome? 91009) false))
    )
  (testing "Palindrome? True"
    (is (= (palindrome? 9009) true))
    ))
