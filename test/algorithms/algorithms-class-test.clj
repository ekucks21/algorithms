(ns algorithms.algorithms-class-test
  (:require [algorithms.algorithms-class :as sut]
            [clojure.test :as t]))

(t/deftest recursive-multiplication-test
  (t/is (= (sut/recursive-multiply 25 29) 725N))
  (t/is (= (sut/recursive-multiply 3141592653589793238462643383279502884197169399375105820974944592 2718281828459045235360287471352662497757247093699959574966967627) 8539734222673567065463550869546574495034888535765114961879601127067743044893204848617875072216249073013374895871952806582723184)))

(t/deftest num-inversions-test
  (t/is (= (sut/num-inversions [2 4 3 5 1]) 5))
  (t/is (= (sut/num-inversions [1 3 2]) 2)))
