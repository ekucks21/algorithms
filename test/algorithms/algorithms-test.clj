(ns algorithms.algorithms-class-test
  (:require [algorithms.algorithms-class :as sut]
            [clojure
             [string :as s]
             [test :as t]]
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

(defn get-ints [file-name]
  (with-open [integersReader (io/reader
                              (io/resource file-name))]
    (doall (map
            #(Integer/valueOf %)
            (line-seq integersReader)))))

(t/deftest count-comparisons-test
  (let [pivot-last (fn [xs] (conj (vec (cons (last xs) (rest (butlast xs)))) (first xs)))
        pivot-median-of-three (fn [xs]
                                (let [indexed-xs (vec (map-indexed vector xs))
                                      pivot (second
                                             (sort #(compare (second %1) (second %2))
                                                   [(first indexed-xs)
                                                    (if (odd? (count indexed-xs))
                                                      (indexed-xs (quot (count indexed-xs) 2))
                                                      (indexed-xs (dec (quot (count indexed-xs) 2))))
                                                    (last indexed-xs)]))]
                                  ;; (cons pivot (remove (partial = pivot) xs))
                                  (assoc xs 0 (second pivot) (first pivot) (first xs))
                                  ))]
    (t/is (= (sut/comparison-count [2 4 3 5 1]) [7 [1 2 3 4 5]]))
    (t/is (= (first (sut/comparison-count [4 9 2 0 8 7])) 9))
    (t/is (= (first (sut/comparison-count [4 9 2 0 8 7 1 6])) 13))
    (t/is (= (first (sut/comparison-count [3 9 8 4 6 10 2 5 7 1])) 25))
    (t/is (= (first (sut/comparison-count (get-ints "100.txt"))) 615))
    (t/is (= (first (sut/comparison-count (get-ints "QuickSortUnsorted.txt"))) 162085))
    (t/is (= (first (sut/comparison-count [2 4 3 5 1] pivot-last)) 9))
    (t/is (= (first (sut/comparison-count (get-ints "100.txt") pivot-last)) 587))
    (t/is (= (first (sut/comparison-count (get-ints "100.txt") pivot-median-of-three)) 518))
    (t/is (= (first (sut/comparison-count (get-ints "QuickSortUnsorted.txt") pivot-last)) 164123))
    (t/is (= (first (sut/comparison-count [2 4 3 5 1] pivot-median-of-three)) 6))
    (t/is (= (first (sut/comparison-count (get-ints "QuickSortUnsorted.txt") pivot-median-of-three)) 138382))))

(defn get-graph [file-name]
  (with-open [r (io/reader (io/resource file-name))]
    (let [ints (map #(map
                      (fn [s] (Integer/parseInt s))
                      (s/split % #"\s+")) (line-seq r))]
      (into [] ints))))

(t/deftest min-cut-test
  (t/is (= (sut/min-cut (get-graph "kargerMinCut.txt")) 5)))

(t/deftest min-cut-small-test
  (t/is (= (sut/min-cut (get-graph "smallKargerMinCut2.txt")) 2))
  (t/is (= (sut/min-cut (get-graph "smallKargerMinCut2.txt")) 3))
  (t/is (= (sut/min-cut (get-graph "smallKargerMinCut2.txt")) 4)))

(t/deftest find-test
  (t/is (= (sut/find-root [{"parent" 1} {"parent" 2} {"parent" 2}] 0)
           [[{"parent" 2} {"parent" 2} {"parent" 2}] 2])))

(t/deftest union-test
  (t/is (= (sut/union [{"parent" 1 "rank" 0} {"parent" 1 "rank" 1}
                       {"parent" 3 "rank" 0} {"parent" 3 "rank" 1}]
                      0 2)
           [{"parent" 1 "rank" 0} {"parent" 3 "rank" 2}
            {"parent" 3 "rank" 0} {"parent" 3 "rank" 1}])))

(t/deftest g->edges
  (t/is (= (sut/g->edges [[0 2 4] [1 2] [2 0 1] [3 4] [4 0 3]])
           #{#{0 2} #{0 4} #{1 2} #{3 4}})))
