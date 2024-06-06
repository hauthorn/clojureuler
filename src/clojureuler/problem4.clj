(ns clojureuler.problem4)

(defn num-to-seq
  "Converts a number into a seq"
  [num]
  (for [n (str num)]
    (- (byte n) 48)))

(defn palindrome?
  "Is the number a palindrome?"
  [num]
  (let [num-as-seq (num-to-seq num)]
    (= num-as-seq (reverse num-as-seq))))


(defn largest-palindrome
  [min max]
  (let [products (for [x (range min (inc max))
                       y (range min (inc max))]
                   (* x y))
        palindromes (filter palindrome? products)]
    (last (sort palindromes))
    ))