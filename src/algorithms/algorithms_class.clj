(ns algorithms.algorithms-class
  (:require [clojure.math.numeric-tower :as math]))

(defn recursive-multiply [x y]
  (if (and (< x 10) (< y 10))
    (* x y)
    (let [num->digits (fn [z]
                        (->> z
                             (iterate #(quot % 10))
                             (take-while (partial < 0))
                             (map #(mod % 10))
                             (reverse)))
          digits->num (fn [digits]
                        (->> digits
                             (reverse)
                             (map-indexed (fn [i digit] (* digit (math/expt 10 i))))
                             (apply +)))
          split-digits (fn [digits length]
                         (map
                          (partial digits->num)
                          (split-at
                           (quot length 2)
                           (concat (repeat (- length (count digits)) 0) digits))))
          x-digits (num->digits x)
          y-digits (num->digits y)
          raw-n (max (count x-digits) (count y-digits))
          n (+ raw-n (mod raw-n 2))
          [a b] (split-digits x-digits n)
          [c d] (split-digits y-digits n)]
      (+
       (* (math/expt 10 n) (recursive-multiply a c))
       (* (math/expt 10 (/ n 2)) (+ (recursive-multiply a d) (recursive-multiply b c)))
       (recursive-multiply b d)))))

(defn num-inversions [xs]
  (let [num-inversions-sort (fn [xs]
                              (let [xs-count (count xs)]
                                (if (= 1 xs-count)
                                  {:count 0 :sorted xs}
                                  (let [{left-count :count
                                         left-sorted :sorted}
                                        (num-inversions (take (quot xs-count 2) xs))

                                        {right-count :count
                                         right-sorted :sorted}
                                        (num-inversions (drop (quot xs-count 2) xs))

                                        {split-count :count
                                         split-sorted :sorted}
                                        (count-split-inv left-sorted right-sorted)]
                                    {:count (+ left-count right-count split-count)
                                     :sorted split-sorted}))))
        count-split-inv (fn [left-sorted right-sorted]
                          (iterate (fn [[left-sorted right-sorted sorted count]])))]
    (:count (num-inversions-sort xs))))
