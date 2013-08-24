(ns cellularview.core)

(defn log [& more]
  (.log js/console (apply str more)))

(defn prettify [obj]
  (js/JSON.stringify obj nil 2))

(defn jsonlog [obj]
  (log (prettify obj)))

(defn hello-world
  []
  (js/alert "Hello, world!"))

(defn fill-rect
  [ctx x y width height r g b]
  (set! (.-fillStyle ctx) (str "rgb(" r "," g "," b ")"))
  (.fillRect ctx x y width height))

(defn render
  [canvas]
  (let [ctx (.getContext canvas "2d")]
    (.clearRect ctx 0 0 (.-width canvas) (.-height canvas))
    (let [image-data (.createImageData ctx 500 500)
          num-tile-rows 4
          num-tile-cols 4
          tile-width (/ (.-width image-data) num-tile-cols)
          tile-height (/ (.-height image-data) num-tile-rows)]
      (doseq [j (range num-tile-rows)
              i (range num-tile-cols)]
        (let [r (int (* (Math/random) 255))
              g (int (* (Math/random) 255))
              b (int (* (Math/random) 255))]
          (fill-rect ctx (* i tile-width) (* j tile-height) tile-width tile-height r g b))))))
            
          
        
    
  


(defn init
  "Given an atom containing a seq of boids, initialize the view"
  [
 ;  cells-atom
   ]
  (let [canvas (.createElement js/document "canvas")]
    (.setAttribute canvas "width" (.-innerWidth js/window))
    (.setAttribute canvas "height" (.-innerHeight js/window))
    (.appendChild (.-body js/document) canvas)
 ;   (add-watch cells-atom :renderer (fn [_ _ _ flock]
 ;                                     (render canvas flock)))))
 (render canvas)
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

(comment
(defn tick
  "The main 'loop' of the simulation."
  [options-atom cells-atom]
  (swap! cells-atom (partial update-flock @options-atom))
  (requestAnimationFrame #(tick options-atom cells-atom))))

(defn main
  []
  (init))

(comment
(defn main
  "Starts everything"
  []
  (let [cells-atom (atom (repeatedly 15 create-boid))]
    (init cells-atom)
    (tick cells-atom))))