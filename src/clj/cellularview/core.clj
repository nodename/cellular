(ns cellularview.core
  (:use [mikera.cljutils.error])
  (:require [clojure.core.async :refer [<! go chan timeout]]
            [cellular.forestfire :refer [simulate-forestfire]]
            [utils.helpers :refer [buffered-chan]]
            [mikera.image.core :as img]
            [mikera.image.colours :as col])
  (:import [java.awt.image BufferedImage]
           [mikera.gui Frames JIcon]
           [java.awt.event WindowListener]
           [javax.swing JComponent JLabel JPanel JFrame]))

;; Java interop code in this namespace proudly stolen from Mike Anderson:
;; https://github.com/mikera/singa-viz/blob/develop/src/main/clojure/mikera/singaviz/main.clj

(defn col ^long
  [state]
  (condp = state
    :alive 0xff228b22
    :burning 0xffff4500
    :dead 0xff5c4033))

(defn cells->image ^BufferedImage
  [cells]
  (let [w (count cells)
        h w
        ^BufferedImage bi (img/new-image w h)]
    (dotimes [y h]
      (dotimes [x w]
        (.setRGB bi (int x) (int y) (unchecked-int (col (get-in cells [y x]))))))
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
        new-frame? (if (not (identical? fr @last-window)) (do (reset! last-window fr) true) false)]
    (when (and on-close new-frame?)
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
     (show (frame bi#) :on-close #(reset! running false))))

(defn run
  [q m]
  (reset! running true)
  (let [{:keys [request response]} (buffered-chan (simulate-forestfire q m) 30)]
    
    (go (while @running
          (<! (timeout 600))
          (>! request :ready)
          (show-simulation-frame response))))
  
  nil)

(defn main
  []
  (run 20 6))
