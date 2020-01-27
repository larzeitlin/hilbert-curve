(ns hilbert-curve.drawing
  (:require [quil.core :as q]))

(defn point-to-pix-converter [iterations width margin]
  (let [root-n-points (Math/pow 2 iterations)
        px-by-point (apply merge
                           (for [x (range root-n-points)
                                 y (range root-n-points)
                                 :let [grid-size (- width (* 2 margin))
                                       unit-size (/ grid-size root-n-points)
                                       adjust-to-center 0.5]]
                             {[x y] [(+ margin (* unit-size (+ adjust-to-center x)))
                                     (+ margin (* unit-size (+ adjust-to-center y)))]}))]
    (fn [[x y]] (get px-by-point [x y]))))

(defn draw-line [points]
  (when (seq points)
    (q/fill 0)
    (q/stroke 0)
    (q/stroke-weight 5)
    (doall (map (fn [[[ax ay] [bx by]]]
                  (q/line ax ay bx by))
                (partition 2 1 points)))))
