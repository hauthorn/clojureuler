(ns clojureuler.problem54
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

; Poker hands https://projecteuler.net/problem=54

; "Domain"
(def suits (sorted-map \S :spades
                       \H :hearts
                       \D :diamonds
                       \C :clubs))
(def ranks (sorted-map \A :ace
                       \K :king
                       \Q :queen
                       \J :jack
                       \T :ten
                       \9 :nine
                       \8 :eight
                       \7 :seven
                       \6 :six
                       \5 :five
                       \4 :four
                       \3 :three
                       \2 :two))

(def ranks-to-int {
                   :ace   14
                   :king  13
                   :queen 12
                   :jack  11
                   :ten   10
                   :nine  9
                   :eight 8
                   :seven 7
                   :six   6
                   :five  5
                   :four  4
                   :three 3
                   :two   2})

(defn rank [card]
  (ranks-to-int (:rank card)))

(defrecord Card [rank suit])

(defn card-counts [hand]
  (let [freqs (frequencies (map :rank hand))
        counts (vals freqs)]
    counts))

(defn same-suit?
  "Are all cards in the hand the same suit?"
  [hand]
  (= 1 (count (set (map :suit hand)))))

(defn royal-flush? [hand]
  (let [hand-ranks (set (map :rank hand))]
    (and (same-suit? hand)
         (= hand-ranks #{:ace :king :queen :jack :ten}))))

(defn straight-flush? [hand]
  (let [hand-ranks (set (map :rank hand))
        rank-distances (for [x hand-ranks y hand-ranks]
                         (abs (- (get ranks-to-int x) (get ranks-to-int y))))]
    (and (same-suit? hand)                                  ; All same suit
         (= 5 (count (set hand-ranks)))                     ; Five distinct ranks/values
         (< (apply max rank-distances) 5))))                ; Cards cannot be 5 away from each other

(defn four-of-a-kind? [hand]
  (let [counts (card-counts hand)
        max-count (apply max counts)]
    (= 4 max-count)))

(defn full-house? [hand]
  (let [counts (card-counts hand)
        max-count (apply max counts)
        min-count (apply min counts)]
    (and (= max-count 3) (= min-count 2))))

(defn flush? [hand]
  (same-suit? hand))

(defn straight? [hand]
  (let [hand-ranks (set (map :rank hand))
        rank-distances (for [x hand-ranks y hand-ranks]
                         (abs (- (get ranks-to-int x) (get ranks-to-int y))))]
    (and (= 5 (count (set hand-ranks)))
         (< (apply max rank-distances) 5))))

(defn three-of-a-kind? [hand]
  (let [counts (card-counts hand)
        max-count (apply max counts)]
    (= max-count 3)))

(defn two-pairs? [hand]
  (let [counts (card-counts hand)]
    (= (sort counts) [1 2 2])))

(defn one-pair? [hand]
  (let [counts (card-counts hand)
        max-count (apply max counts)]
    (= max-count 2)))

(defn highest-card? [hand1 hand2]
  (loop [hand1 (sort #(> (rank %1) (rank %2)) hand1)        ; Sort highest number first
         hand2 (sort #(> (rank %1) (rank %2)) hand2)]
    (if (empty? hand1)
      false                                                 ; If empty, they must have had the same ranks, we didn't win
      (cond
        (> (rank (first hand1)) (rank (first hand2))) true
        (< (rank (first hand1)) (rank (first hand2))) false
        (= (rank (first hand1)) (rank (first hand2))) (recur (rest hand1) (rest hand2))))))

; I got lazy and started to duplicate code because it's tedious
(defn best-hand [hand1 hand2 hand-type]
  (case hand-type
    four-of-a-kind? (let [rank1 (->> hand1
                                     (map :rank)
                                     (frequencies)
                                     (filter #(= 4 (val %)))
                                     (first)
                                     (key)
                                     (ranks-to-int))
                          rank2 (->> hand2
                                     (map :rank)
                                     (frequencies)
                                     (filter #(= 4 (val %)))
                                     (first)
                                     (key)
                                     (ranks-to-int))]
                      (if (= rank1 rank2)
                        (highest-card? hand1 hand2)
                        (> rank1 rank2)))

    full-house? (let [three-kind-rank1 (->> hand1
                                            (map :rank)
                                            (frequencies)
                                            (filter #(= 3 (val %)))
                                            (first)
                                            (key)
                                            (ranks-to-int))
                      three-kind-rank2 (->> hand2
                                            (map :rank)
                                            (frequencies)
                                            (filter #(= 3 (val %)))
                                            (first)
                                            (key)
                                            (ranks-to-int))]
                  (if (= three-kind-rank1 three-kind-rank2)
                    (let [pair-rank1 (->> hand1
                                          (map :rank)
                                          (frequencies)
                                          (filter #(= 2 (val %)))
                                          (first)
                                          (key)
                                          (ranks-to-int))
                          pair-rank2 (->> hand2
                                          (map :rank)
                                          (frequencies)
                                          (filter #(= 2 (val %)))
                                          (first)
                                          (key)
                                          (ranks-to-int))]
                      (> pair-rank1 pair-rank2))
                    (> three-kind-rank1 three-kind-rank2)))

    three-of-a-kind? (let [rank1 (->> hand1
                                      (map :rank)
                                      (frequencies)
                                      (filter #(= 3 (val %)))
                                      (first)
                                      (key)
                                      (ranks-to-int))
                           rank2 (->> hand2
                                      (map :rank)
                                      (frequencies)
                                      (filter #(= 3 (val %)))
                                      (first)
                                      (key)
                                      (ranks-to-int))]
                       (if (= rank1 rank2)
                         (highest-card? hand1 hand2)
                         (> rank1 rank2)))

    two-pairs? (let [pairs1 (->> hand1
                                 (map :rank)
                                 (frequencies)
                                 (filter #(= 2 (val %)))
                                 (map key)
                                 (sort #(> (ranks-to-int %1) (ranks-to-int %2))))
                     pairs2 (->> hand2
                                 (map :rank)
                                 (frequencies)
                                 (filter #(= 2 (val %)))
                                 (map key)
                                 (sort #(> (ranks-to-int %1) (ranks-to-int %2))))
                     high-pair1 (first pairs1)
                     high-pair2 (first pairs2)
                     low-pair1 (second pairs1)
                     low-pair2 (second pairs2)]
                 (if (= (ranks-to-int high-pair1) (ranks-to-int high-pair2))
                   (if (= (ranks-to-int low-pair1) (ranks-to-int low-pair2))
                     (highest-card? hand1 hand2)
                     (> (ranks-to-int low-pair1) (ranks-to-int low-pair2)))
                   (> (ranks-to-int high-pair1) (ranks-to-int high-pair2))))

    one-pair? (let [rank1 (->> hand1
                               (map :rank)
                               (frequencies)
                               (filter #(= 2 (val %)))
                               (first)
                               (key)
                               (ranks-to-int))
                    rank2 (->> hand2
                               (map :rank)
                               (frequencies)
                               (filter #(= 2 (val %)))
                               (first)
                               (key)
                               (ranks-to-int))]
                (if (= rank1 rank2)
                  (highest-card? hand1 hand2)
                  (> rank1 rank2)))))

(defn winning-hand?
  "Is hand 1 winning over hand 2?"
  [hand1 hand2]
  (cond
    (royal-flush? hand1) (not (royal-flush? hand2))         ; Royal flush wins unless the other has one
    (royal-flush? hand2) false                              ; Hand2 is a royal flush, we lose

    (straight-flush? hand1) (if (straight-flush? hand2) (highest-card? hand1 hand2) true)
    (straight-flush? hand2) false

    (four-of-a-kind? hand1) (if (four-of-a-kind? hand2) (best-hand hand1 hand2 'four-of-a-kind?) true)
    (four-of-a-kind? hand2) false

    (full-house? hand1) (if (full-house? hand2) (best-hand hand1 hand2 'full-house?) true)
    (full-house? hand2) false

    (flush? hand1) (if (flush? hand2) (highest-card? hand1 hand2) true)
    (flush? hand2) false

    (straight? hand1) (if (straight? hand2) (highest-card? hand1 hand2) true)
    (straight? hand2) false

    (three-of-a-kind? hand1) (if (three-of-a-kind? hand2) (best-hand hand1 hand2 'three-of-a-kind?) true)
    (three-of-a-kind? hand2) false

    (two-pairs? hand1) (if (two-pairs? hand2) (best-hand hand1 hand2 'two-pairs?) true)
    (two-pairs? hand2) false

    (one-pair? hand1) (if (one-pair? hand2) (best-hand hand1 hand2 'one-pair?) true)
    (one-pair? hand2) false

    :else (highest-card? hand1 hand2)))


; Utilities for parsing the hands

(defn string-to-card
  "Maps a string like TD to Card [:queen :diamonds]"
  [string]
  (let [[rank suit] (seq string)]
    (->Card (ranks rank) (suits suit))))

(defn map-hand
  "Map a line of the text file into a pair of poker hands"
  [line]
  (let [chunks (str/split line #" ")
        cards (map string-to-card chunks)]
    (partition 5 cards)))

(def hands (with-open [rdr (io/reader "0054_poker.txt")]
             (mapv map-hand (line-seq rdr))))

(defn main-
  [& _]
  (println "Wins: " (count (filter true? (map #(winning-hand? (first %) (second %)) hands)))))