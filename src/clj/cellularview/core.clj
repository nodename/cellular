(ns cellularview.core
  (:use [mikera.cljutils.error])
  (:require [clojure.core.async :refer [<! go timeout]]
            [cellular.forestfire :refer [simulate-forestfire]]
            [cellular.laplace :refer [simulate-laplace]]
            [utils.helpers :refer [buffered-chan]]
            [mikera.image.core :as img]
            [mikera.image.colours :as col])
  (:import [java.awt.image BufferedImage]
           [mikera.gui Frames JIcon]
           [java.awt.event WindowListener]
           [javax.swing JComponent JLabel JPanel JFrame]))

;; Java interop code in this namespace proudly stolen from Mike Anderson:
;; https://github.com/mikera/singa-viz/blob/develop/src/main/clojure/mikera/singaviz/main.clj

(defn double-color ^long
  [value]
  (cond
    (<= value 0.0) 0xFF000000
    (<= value 100.0) (let [v (/ value 100.0)] (col/rgb v v v))
    :else 0xFFFFFFFF))

(defmulti color class)
(defmethod color clojure.lang.Keyword
  [state]
  (condp = state
    :alive 0xff228b22
    :burning 0xffff4500
    :dead 0xff5c4033))

(defmethod color Long
  [value]
  (double-color value))

(defmethod color Double
  [^double value]
  (double-color value))

(defn cells->image ^BufferedImage
  [cells]
  (let [w (count cells)
        h w
        ^BufferedImage bi (img/new-image w h)]
    (dotimes [y h]
      (dotimes [x w]
        (.setRGB bi (int x) (int y) (unchecked-int (color (get-in cells [y x]))))))
    bi))

(defn component
  "Creates a component as appropriate to visualise an object x"
  (^JComponent [x]
    (cond
      (instance? BufferedImage x) (JIcon. ^BufferedImage x)
      :else (error "Don't know how to visualize: " x))))

(def last-window (atom nil))

(defn show
  "Shows a component in a new frame"
  ([com
    & {:keys [^String title on-close]
       :as options
       :or {title nil}}]
  (let [com (component com)
        ^JFrame fr (Frames/display com (str title))
      ;  new-frame? (if (not (identical? fr @last-window)) (do (reset! last-window fr) true) false)
        ]
   ; (when (and on-close new-frame?)
    (when on-close
      (.addWindowListener fr (proxy [WindowListener] []
                               (windowActivated [e])
                               (windowClosing [e]
                                 (on-close))
                               (windowDeactivated [e])
                               (windowDeiconified [e])
                               (windowIconified [e])
                               (windowOpened [e])
                               (windowClosed [e])))))))

(def FRAMESIZE 800)

(defn frame
  [^BufferedImage bi]
  (let [factor (/ FRAMESIZE (.getWidth bi))]
    (img/zoom factor bi)))

(def running (atom true))

(defmacro show-simulation-frame
  [simulation]
  `(let [grid# ((<! ~simulation) :grid)
         bi# (cells->image grid#)]
     (show (frame bi#) :title "Relax" :on-close #(reset! running false))))

(defn run
  [simulate q m]
  (reset! running true)
  (let [{:keys [request response]} (buffered-chan (simulate q m) 30)]
    
    (go (while @running
          (<! (timeout 750))
          (>! request :ready)
          (show-simulation-frame response))))
  
  nil)

(defn main
  []
;  (run simulate-forestfire 20 8))
  (run simulate-laplace 5 32))
