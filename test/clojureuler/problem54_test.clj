(ns clojureuler.problem54-test
  (:require [clojure.test :refer :all])
  (:require [clojureuler.problem54 :refer :all]))


; Fixtures
(def hand-one (list (->Card :three :diamonds)
                    (->Card :ten :spades)
                    (->Card :four :diamonds)
                    (->Card :two :hearts)
                    (->Card :six :hearts)))

(def four-aces (list (->Card :ace :diamonds)
                     (->Card :six :spades)
                     (->Card :ace :clubs)
                     (->Card :ace :hearts)
                     (->Card :ace :spades)))

(def hand-two (list (->Card :five :clubs)
                    (->Card :nine :clubs)
                    (->Card :seven :clubs)
                    (->Card :eight :clubs)
                    (->Card :six :clubs)))

(def royal-flush-hearts (list (->Card :king :hearts)
                              (->Card :queen :hearts)
                              (->Card :ace :hearts)
                              (->Card :ten :hearts)
                              (->Card :jack :hearts)))

(def two-pairs-hand (list (->Card :three :diamonds)
                          (->Card :three :spades)
                          (->Card :four :diamonds)
                          (->Card :four :hearts)
                          (->Card :six :hearts)))

(deftest string-to-hand-test
  (testing "String to hand"
    (is (= (string-to-card "KD") (->Card :king :diamonds)))
    (is (= (string-to-card "2C") (->Card :two :clubs)))
    ))

(deftest map-hands-test
  (testing "Map line to hands"
    (is (= (map-hand "3D TS 4D 2H 6H 5C 9C 7C 8C 6C")
           (list hand-one hand-two)))
    ))


(deftest same-suit?-test
  (testing "All cards in the hand are the same suit"
    (is (true? (same-suit? royal-flush-hearts)))
    (is (false? (same-suit? hand-one)))
    (is (true? (same-suit? hand-two)))
    ))

(deftest royal-flush?-test
  (testing "Royal flush?"
    (is (true? (royal-flush? royal-flush-hearts)))
    (is (false? (royal-flush? hand-two)))
    ))

(deftest straight-flush?-test
  (testing "Straight flush"
    (is (true? (straight-flush? hand-two)))))

(deftest four-of-a-kind?-test
  (testing "Four of a kind"
    (is (true? (four-of-a-kind? four-aces)))
    (is (false? (four-of-a-kind? hand-one)))
    ))

(deftest two-pairs?-test
  (testing "Two pairs"
    (is (true? (two-pairs? two-pairs-hand)))))

(deftest highest-card?-test
  (testing "Highest card of royal flush and four aces"
    (is (true? (highest-card? four-aces royal-flush-hearts))))
  (testing "Ten should be higher than 9"
    (is (true? (highest-card? hand-one hand-two))))
  (testing "Two equal hands should return false"
    (is (false? (highest-card? hand-one hand-one))))
  )

(deftest winning-hand?-test
  (testing "Winning hand: Royal flush"
    (is (true? (winning-hand? royal-flush-hearts hand-two)))
    ))

(deftest best-hand-test
  (testing "Best hand both two pairs"
    (is (true? (best-hand two-pairs-hand (list (->Card :three :diamonds)
                                               (->Card :three :spades)
                                               (->Card :two :diamonds)
                                               (->Card :two :hearts)
                                               (->Card :six :hearts)) 'two-pairs?))))
  )
