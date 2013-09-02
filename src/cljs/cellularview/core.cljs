(ns cellularview.core
  (:require [cljs.core.async :refer [<!]]
            [cellular.forestfire :refer [simulate-forestfire]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn log [& more]
  (.log js/console (apply str more)))

(defn color
  [state]
  (condp = state
    :alive {:r 0 :g 255 :b 0}
    :burning {:r 255 :g 0 :b 0}
    :dead {:r 139 :g 69 :b 19}))

(defn fill-rect
  [ctx x y width height r g b]
  (set! (.-fillStyle ctx) (str "rgb(" r "," g "," b ")"))
  (.fillRect ctx x y width height))

(defn render
  [canvas cells]
  (let [ctx (.getContext canvas "2d")]
    (.clearRect ctx 0 0 (.-width canvas) (.-height canvas))
    (let [image-data (.createImageData ctx 600 600)
          num-tile-rows (count cells)
          num-tile-cols (count cells) ;; we assume cells is square
          tile-width (/ (.-width image-data) num-tile-cols)
          tile-height (/ (.-height image-data) num-tile-rows)]
      (doseq [j (range num-tile-rows)
              i (range num-tile-cols)]
        (let [cell (get-in cells [i j])
              {:keys [r g b]} (color cell)]
          (fill-rect ctx (* i tile-width) (* j tile-height) tile-width tile-height r g b))))))

(defn init
  "Given an atom, initialize the view"
  [cells-atom]
  (let [canvas (.createElement js/document "canvas")]
    (.setAttribute canvas "width" (.-innerWidth js/window))
    (.setAttribute canvas "height" (.-innerHeight js/window))
    (.appendChild (.-body js/document) canvas)
  ;  (add-watch cells-atom :renderer (fn [_ _ _ cells] (render canvas cells)))
    
  canvas
    ))

(defn requestAnimationFrame
  "Cross-browser wrapper for requestAnimationFrame"
  [callback]
  (cond
   (.-requestAnimationFrame js/window)
   (.requestAnimationFrame js/window callback)
   (.-webkitRequestAnimationFrame js/window)
   (.webkitRequestAnimiationFrame js/window callback)
   (.-mozRequestAnimationFrame js/window)
   (.mozRequestAnimationFrame js/window callback)
   (.-msRequestAnimationFrame js/window)
   (.msRequestAnimationFrame js/window callback)))

;(comment
;(defn run
;  "The main 'loop' of the simulation."
;  [q m steps cells-atom]
;  (let [simulation (simulate-forestfire q m steps)
;        tick (fn tick [cells-atom]
;               (let [grid ((<!! simulation) :grid)]
;                 (log grid)
;               (reset! cells-atom grid)
;               (requestAnimationFrame #(tick cells-atom))))]
;    nil)))

(defn main
  []
  (let [canvas (init nil)
        cells (atom [[:alive :alive :alive] [:alive :alive :alive] [:alive :alive :dead]])
        simulation (simulate-forestfire 10 4 5)]
    (comment
    (go
      (reset! cells ((<! simulation) :grid))
      (reset! cells ((<! simulation) :grid))
      (reset! cells ((<! simulation) :grid))
      (render canvas @cells)))
    (go
      (log ((<! simulation) :elapsed-ms))
      (log ((<! simulation) :elapsed-ms))
      (log ((<! simulation) :elapsed-ms))
      (log ((<! simulation) :elapsed-ms)))))
  

;(comment
;(defn main
;  "Starts everything"
;  []
;  (let [cells-atom (atom [])]
;    (init cells-atom)
;    (run 3 2 10 cells-atom))))