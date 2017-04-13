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
  (let [count-split-inv
        (fn [left-sorted right-sorted]
               (let [total-count (+ (count left-sorted) (count right-sorted))]
            (first
             (filter
              (fn [[_ _ sorted _]] (= (count sorted) total-count))
              (iterate
               (fn [[[left-first & left-rest :as left-sorted]
                     [right-first & right-rest :as right-sorted]
                     sorted
                     count-inversions]]
                 (cond
                     (nil? left-first) [nil nil (into sorted right-sorted) count-inversions]
                     (nil? right-first) [nil nil (into sorted left-sorted) count-inversions]
                     (<= left-first right-first) [left-rest right-sorted (conj sorted left-first) count-inversions]
                     (< right-first left-first) [left-sorted right-rest (conj sorted right-first) (+ count-inversions (count left-sorted))]))
               [left-sorted right-sorted [] 0])))))
        num-inversions-sort
        (fn num-inversions-sort [xs]
          (let [xs-count (count xs)]
                 (if (= 1 xs-count)
                   {:count 0 :sorted xs}
                   (let [{left-count :count
                          left-sorted :sorted}
                         (num-inversions-sort (take (quot xs-count 2) xs))

                         {right-count :count
                          right-sorted :sorted}
                         (num-inversions-sort (drop (quot xs-count 2) xs))

                         [_ _ split-sorted split-count]
                         (count-split-inv left-sorted right-sorted)]
                     {:count (+ left-count right-count split-count)
                      :sorted split-sorted}))))]
    (:count (num-inversions-sort xs))))

(defn- quicksort-partition [[pivot & unpartitioned]]
  (let [[left right]
        (reduce
         (fn [[left right] x]
           (if (< x pivot)
             [(conj left x)
              (if (first right)
                (conj (vec (rest right)) (first right))
                right)]
             [left
              (conj right x)]))
         [[] []] unpartitioned)]
    (do
      ;; (println [(count unpartitioned)
      ;;           (if (last left)
      ;;             (vec (cons (last left) (butlast left)))
      ;;             left)
      ;;           pivot
      ;;           right])
      [(count unpartitioned)
       (if (last left)
         (vec (cons (last left) (butlast left)))
         left)
       pivot
       right])))

(defn comparison-count
  ([xs] (comparison-count xs identity))
  ([xs pivot-picker]
   (let [xs-vec (vec xs)]
     (if (<= (count xs-vec) 1)
       [0 xs-vec]
       (let [[comp-count left pivot right] (quicksort-partition (pivot-picker xs-vec))
             [left-comp-count left-sorted] (comparison-count left pivot-picker)
             [right-comp-count right-sorted] (comparison-count right pivot-picker)]
         [(+ comp-count left-comp-count right-comp-count)
          (into (conj left-sorted pivot) right-sorted)])))))

(defn min-cut [g]
  (let [n (count g)
        edges (apply concat (map (fn [[key value]] (map #(vector key %) value)) g))
        random-contract (fn [g]
                          (let [[v1 v2 :as edge-to-contract] (repeatedly 2 (inc (rand-int n)))]
                            (reduce-kv
                             #(assoc %1 %2 ()) {} (dissoc g v1))))
        random-contract-min-cut (fn random-contract-min-cut [g]
                                  (if (= 2 (count g))
                                    g
                                    (random-contract-min-cut (random-contract g))))]
    (min (repeatedly (* (* n n) (Math/log n)) #(random-contract-min-cut g)))))
