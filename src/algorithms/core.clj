(ns algorithms.core
  (:gen-class)
  (:require [clojure.string :as str]))

 (defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn fizzbuzz []
  (range 1 100))

(->> (range 1 100)
     (map #(cond
               (and (zero? (mod % 3)) (zero? (mod % 5))) "fizzbuzz"
               (zero? (mod % 3)) "fizz"
               (zero? (mod % 5)) "buzz"
               :else %)))

(defn tic-tac-toe-win? [board]
  (let [win-across (some #(or (= #{"x"} %) (= #{"o"} %)) (map set board))
        win-down (some #(or (= #{"x"} %) (= #{"o"} %)) (apply map (partial conj #{}) board))
        win-diagonal (some
                      #(or (= #{"x"} %) (= #{"o"} %))
                      [#{(first (first board)) (second (second board)) (nth (nth board 2) 2)}
                       #{(first (nth board 2)) (second (second board)) (nth (first board) 2)}])]
    (or win-across win-down win-diagonal)))

(defn fibonacci []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1])))

(defn insert-sort [coll]
  (loop [sortedIndex 0 beingSorted coll]
    (keep-indexed #(if (> %2)) (rseq beingSorted))))

(defn merge-sort [coll]
  (if (or (empty? coll) (= 1 (count coll)))
    coll
    (let [[left right] (split-at (/ (count coll) 2) coll)]
      (loop [sorted [] left (merge-sort left) right (merge-sort right)]
        (cond
          (empty? left) (into sorted right)
          (empty? right) (into sorted left)
          :else  (if (> 0 (compare (first left) (first right)))
                   (recur (conj sorted (first left)) (rest left) right)
                   (recur (conj sorted (first right)) left (rest right))))))))

(defn merge-step
  ([seq1 seq2]
   (merge-step [] seq1 seq2))
  ([result seq1 seq2]
   (let [head1 (first seq1)
         head2 (first seq2)]
     (cond
       (nil? head1) (concat result seq2)
       (nil? head2) (concat result seq1)
       :else (do
               (println (str/join " " ["processed" head1 head2]))
               (if (< head1 head2)
                 (recur (conj result head1) (rest seq1) seq2)
                 (recur (conj result head2) seq1 (rest seq2))))))))

(defn divide-step [coll]
  (let [coll-length (count coll)]
    (if (> coll-length 1)
      (let [[seq1 seq2] (split-at (quot coll-length 2) coll)]
        (merge-step (divide-step seq1) (divide-step seq2)))
      coll)))

(defn lazy-sort
  ([input-seq]
   (lazy-sort
    (merge-step [(first input-seq)] [(second input-seq)])
    (drop 2 input-seq)))
  ([partial-result input-seq]
   (let [result-length (count partial-result)]
     (if (seq input-seq)
       (recur
        (merge-step partial-result (divide-step (take result-length input-seq)))
        (drop result-length input-seq))
       partial-result))))

(defn fizz-buzz [n] (condp
                        (fn [a b] (zero? (mod b a))) n
                      15 "fizzbuzz"
                      3 "fizz"
                      5 "buzz"
                      n))

(map fizz-buzz (range 1 100))
