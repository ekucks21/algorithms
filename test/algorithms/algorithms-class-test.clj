(ns algorithms.algorithms-class-test
  (:require [algorithms.algorithms-class :as sut]
            [clojure.test :as t]))

(t/deftest recursive-multiplication-test
  (t/is (= (sut/recursive-multiply 25 29) 725.0))
  (t/is (= (sut/recursive-multiply 3141592653589793238462643383279502884197169399375105820974944592 2718281828459045235360287471352662497757247093699959574966967627) 8.539734222673568E126)))

