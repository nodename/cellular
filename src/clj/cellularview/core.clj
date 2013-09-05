(ns cellularview.core
  (:use [mikera.cljutils.error])
  (:require [clojure.core.async :refer [<! go]]
            [cellular.forestfire :refer [simulate-forestfire]]
            [mikera.image.core :as img]
            [mikera.image.colours :as col])
  (:import [java.awt.image BufferedImage]
           [mikera.gui Frames JIcon]
           [java.awt.event WindowListener]
           [javax.swing JComponent JLabel JPanel JFrame]))

;; code in this namespace proudly stolen from Mike Anderson:
;; https://github.com/mikera/singa-viz/blob/develop/src/main/clojure/mikera/singaviz/main.clj

(defn col ^long
  [state]
  (condp = state
    :alive (col/rgb 0.0 1.0 0.0)
    :burning (col/rgb 1.0 0.0 0.0)
    :dead (col/rgb 0.545 0.271 0.075)))

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

(defn run
  [q m]
  (reset! running true)
  (let [simulation (simulate-forestfire q m)]
    (go
      (while @running
        (let [grid ((<! simulation) :grid)
              bi (cells->image grid)]
          (show (frame bi) :on-close #(reset! running false))))))
  nil)

(defn main
  []
  (run 20 6))
