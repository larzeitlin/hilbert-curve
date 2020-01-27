(ns hilbert-curve.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [hilbert-curve.drawing :as d]))

(def rules {:A {:path [[0 0] [0 1] [1 1] [1 0]]
                :next-iteration [:D :A :A :B]}
            :B {:path [[1 1] [0 1] [0 0] [1 0]]
                :next-iteration [:C :B :B :A]}
            :C {:path [[1 1] [1 0] [0 0] [0 1]]
                :next-iteration [:B :C :C :D]}
            :D {:path [[0 0] [1 0] [1 1] [0 1]]
                 :next-iteration [:A :D :D :C]}})

(defn rule-applicator [rules {:keys [cell pattern]}]
  (let [meta-cell (map (partial * 2) cell)
        path (->> rules pattern :path (map (fn [cell] (map + meta-cell cell))))
        cell-patterns (-> rules pattern :next-iteration)]
    (map (fn [cell pattern] {:cell cell :pattern pattern})
         path
         cell-patterns)))

(defn make-points [rules iterations]
  (let [base-case [{:cell [0 0] :pattern :A}]
        resolve-next-layer (fn [cells-to-patterns]
                                (->> cells-to-patterns
                                     (map (partial rule-applicator rules))
                                     (apply concat)))]
    (loop [counter iterations
           cells-to-patterns base-case]
      (if (zero? counter) cells-to-patterns
          (recur (dec counter)
                 (resolve-next-layer cells-to-patterns))))))

(defn hilbert-points-to-canvas [iterations width margin]
  (let [points-to-px-fn (d/point-to-pix-converter iterations
                                                  width
                                                  margin)
        points (make-points rules iterations)
        cells (map :cell points)]
    (map points-to-px-fn cells)))

(defn setup []
  (let [iterations 6
        margin 10
        n-points (Math/pow (Math/pow 2 iterations) 2)]
    (q/frame-rate 100)
    (q/color-mode :hsb)
    {:points-px (vec (hilbert-points-to-canvas iterations (q/width) margin))
     :n-points n-points
     :current-point 1}))

(defn update-state [state]
  (update state :current-point (fn [p] (mod (inc p) (:n-points state)))))

(defn draw-state [state]
  (q/background 240)
  (q/fill 0)
  (d/draw-line (subvec (:points-px state) 0 (:current-point state))))

(q/defsketch hilbert-curve
  :title "Psuedo Hilbert curve"
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
