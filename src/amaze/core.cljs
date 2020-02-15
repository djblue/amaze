(ns amaze.core
  (:require [clojure.set :as set]))

(def canvas-size 800)
(def nubmer-of-cells 16)

(defn neighbors [position]
  (->> (for [[row column] [[-1 0] [0 1] [1 0] [0 -1]]]
         (-> position
             (update :row + row)
             (update :column + column)))
       (filter
        (fn [{:keys [row column]}]
          (and (< -1 row nubmer-of-cells) (< -1 column nubmer-of-cells))))
       set))

(defn generate-maze-iter [state]
  (let [{:keys [stack removed visited]} state]
    (if (empty? stack)
      (assoc state :done? true)
      (let [[current-cell & stack] stack
            unvisited-neighbors
            (set/difference (neighbors current-cell) visited)]
        (if-not (empty? unvisited-neighbors)
          (let [chosen-cell (rand-nth (seq unvisited-neighbors))]
            {:stack (conj (conj stack current-cell) chosen-cell)
             :removed (conj removed #{current-cell chosen-cell})
             :visited (conj visited chosen-cell)})
          (assoc state :stack stack))))))

(defn generate-maze-initial [start]
  {:stack [start] :removed #{} :visited #{start}})

(defn generate-maze [start]
  (loop [state (generate-maze-initial start)]
    (if (:done? state)
      state
      (recur (generate-maze-iter state)))))

(defn cell-points [position]
  (let [{:keys [row column]} position
        scale (/ canvas-size nubmer-of-cells)
        x (* column  scale)
        y (* row scale)]
    #{{:x x :y y}
      {:x x :y (+ y scale)}
      {:x (+ x scale) :y y}
      {:x (+ x scale) :y (+ y scale)}}))

(defn wall-position [wall]
  (let [[a b] (seq wall)]
    (set/intersection (cell-points a) (cell-points b))))

(defn wall-line [wall]
  (let [[begin end] (seq (wall-position wall))
        circle {:type :circle :radius 4 :fill "#202020"}]
    [{:type :line
      :begin begin
      :end end
      :stroke "#202020"
      :stroke-width 3}
     (merge begin circle)
     (merge end circle)]))

(defn cell-rect [position]
  (let [{:keys [row column]} position
        scale (/ canvas-size nubmer-of-cells)]
    {:type :rect
     :fill "#e0245e"
     :x (* column scale)
     :y (* row scale)
     :height scale
     :width scale}))

(def get-all-walls
  (memoize
   (fn get-all-walls []
     (set
      (mapcat
       (fn [position]
         (map #(-> #{position %})
              (neighbors position)))
       (for [row (range nubmer-of-cells)
             column (range nubmer-of-cells)]
         {:row row :column column}))))))

(defn maze [state]
  (let [{:keys [stack removed visited]} state]
    [{:type :rect
      :x 0 :y 0
      :width canvas-size
      :height canvas-size
      :fill "#2e2e2e"
      :stroke-width 3
      :stroke "#2e2e2e"}
     (map cell-rect visited)
     (map #(merge (cell-rect %) {:fill "#262626"}) stack)
     (cell-rect (first stack))
     (map wall-line (set/difference (get-all-walls) removed))]))

(defn set-styles! [ctx shape]
  (when-let [fill (:fill shape)]
    (set! (.-fillStyle ctx) (name fill))
    (.fill ctx))
  (set! (.-lineWidth ctx) 1)
  (when-let [width (:stroke-width shape)]
    (set! (.-lineWidth ctx) width))
  (when-let [stroke (:stroke shape)]
    (set! (.-strokeStyle ctx) (name stroke))
    (.stroke ctx)))

(declare render!)

(defn render-shape! [ctx shape]
  (if (or (vector? shape) (seq? shape))
    (render! ctx shape)
    (do
      (case (:type shape)
        :rect
        (let [{:keys [x y width height]} shape]
          (.beginPath ctx)
          (.moveTo ctx x y)
          (.lineTo ctx x (+ y width))
          (.lineTo ctx (+ x height) (+ y width))
          (.lineTo ctx (+ x height) y)
          (.lineTo ctx x y))
        :line
        (let [{:keys [begin end]} shape]
          (.beginPath ctx)
          (.moveTo ctx (:x begin) (:y begin))
          (.lineTo ctx (:x end) (:y end)))
        :circle
        (let [{:keys [x y radius]} shape]
          (.beginPath ctx)
          (.arc ctx x y radius 0 (* js/Math.PI 2))))
      (set-styles! ctx shape))))

(defn render! [ctx shapes]
  (doseq [shape shapes]
    (render-shape! ctx shape)))

(defonce state (atom nil))

(defn update-loop! []
  (let [canvas (js/document.querySelector "canvas")
        width canvas-size
        height canvas-size
        ctx (.getContext canvas "2d")]
    (set! (.-width canvas) width)
    (set! (.-height canvas) height)
    (.clearRect ctx 0 0 width height)
    (render! ctx (maze @state))))

(defn animation-loop! []
  (swap! state generate-maze-iter)
  (when-not (:done? @state)
    (js/setTimeout #(animation-loop!) 66)))

(defn main! []
  (add-watch state :app-state update-loop!)
  (reset! state (generate-maze-initial {:row 0 :column 0}))
  (animation-loop!))

(defn reload! [] (update-loop!))

