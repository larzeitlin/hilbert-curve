(ns hilbert-curve.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def config {:margin 10})

(def rules {:A {:path [[0 0] [0 1] [1 1] [1 0]]
                :next-iteration [:D :A :A :B]}
            :B {:path [[1 1] [0 1] [0 0] [1 0]]
                :next-iteration [:C :B :B :A]}
            :C {:path [[1 1] [1 0] [0 0] [0 1]]
                :next-iteration [:B :C :C :D]}
            :D {:path [[0 0] [1 0] [1 1] [0 1]]
                 :next-iteration [:A :D :D :C]}})


(defn point-to-pix-converter [root-n-points width margin]
  (let [px-by-point (apply merge
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

(defn pattern-map [pattern-key rules]
  (let [rule (pattern-key rules)]
    (merge (map (fn [p i] {:cell p :pattern i})
                (:path rule)
                (:next-iteration rule)))))

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


(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)
  (let [iterations 6
        points-to-px-fn (point-to-pix-converter (Math/pow 2 iterations)
                                                (q/width)
                                                (:margin config))
        points (make-points rules iterations)
        cells (map :cell points)
        points-px (map points-to-px-fn cells)]
    {:points-px points-px}))

(defn update-state [state]
  state)

(defn draw-state [state]
  (q/background 240)
  (q/fill 0)
  (draw-line (:points-px state)))


(q/defsketch hilbert-curve
  :title "Psuedo Hilbert curve"
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
