(ns clojureuler.problem6)

; Sum square difference

(defn sum-of-squares
  "Calculates the sum of the squares of the natural numbers up to and including n"
  [n]
  (apply + (map #(* % %) (range 1 (inc n)))))

(defn square-of-sums
  "Calculates the square of the sums of the natural numbers up to and including n"
  [n]
  (let [sums (reduce + (range 1 (inc n)))]
    (* sums sums)))