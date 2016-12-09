(ns algorithms.algorithms-class)

(defn recursive-multiply [x y]
  (if (< x 10)
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
                             (map-indexed (fn [i digit] (* digit (Math/pow 10 i))))
                             (apply +)))
          x-digits (num->digits x)
          y-digits (num->digits y)
          n (count x-digits)
          [a b] (map (partial digits->num) (split-at (quot n 2) x-digits))
          [c d] (map (partial digits->num) (split-at (quot n 2) y-digits))]
      (+
       (* (Math/pow 10 n) (recursive-multiply a c))
       (* (Math/pow 10 (quot n 2)) (+ (recursive-multiply a d) (recursive-multiply b c)))
       (recursive-multiply b d)))))
