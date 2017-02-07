(ns algorithms.algorithms-class-test
  (:require [algorithms.algorithms-class :as sut]
            [clojure.test :as t]
            [clojure.java.io :as io]))

(t/deftest recursive-multiplication-test
  (t/is (= (sut/recursive-multiply 25 29) 725N))
  (t/is (= (sut/recursive-multiply 3141592653589793238462643383279502884197169399375105820974944592 2718281828459045235360287471352662497757247093699959574966967627) 8539734222673567065463550869546574495034888535765114961879601127067743044893204848617875072216249073013374895871952806582723184)))

(t/deftest num-inversions-test
  (t/is (= (sut/num-inversions [2 4 3 5 1]) 5))
  (t/is (= (sut/num-inversions [1 3 2]) 1))
  (t/is (= (sut/num-inversions (with-open [integersReader (io/reader
                                                           (io/resource "IntegerArray.txt"))]
                                 (doall (map
                                   #(Integer/valueOf %)
                                   (line-seq integersReader)))))
           2407905288)))

(defn get-quick-sort-unsorted []
  (with-open [integersReader (io/reader
                              (io/resource "QuickSortUnsorted.txt"))]
    (doall (map
            #(Integer/valueOf %)
            (line-seq integersReader)))))

(t/deftest count-comparisons-test
  (let [pivot-last (fn [xs] (concat [(last xs)] (butlast xs)))
        pivot-median-of-three (fn [xs]
                                (let [pivot (second (sort [(first xs)
                                                           (first (drop (quot (count xs) 2) xs))
                                                           (last xs)]))]
                                  (cons pivot (remove (partial = pivot) xs))))]
    (t/is (= (sut/comparison-count [2 4 3 5 1]) [7 [1 2 3 4 5]]))
    (t/is (= (first (sut/comparison-count [4 9 2 0 8 7])) 9))
    (t/is (= (first (sut/comparison-count [4 9 2 0 8 7 1 6])) 14))
    (t/is (= (first (sut/comparison-count [3 9 8 4 6 10 2 5 7 1])) 25))
    (t/is (= (first (sut/comparison-count (get-quick-sort-unsorted))) 163145))
    (t/is (= (first (sut/comparison-count [2 4 3 5 1] pivot-last)) 9))
    (t/is (= (first (sut/comparison-count (get-quick-sort-unsorted) pivot-last)) 149789))
    (t/is (= (first (sut/comparison-count [2 4 3 5 1] pivot-median-of-three)) 7))
    (t/is (= (first (sut/comparison-count (get-quick-sort-unsorted) pivot-median-of-three)) 142758))))

;; first pivot is not 163145
