(ns cellular.forestfire
  (:require [cellular.cellular :refer [simulate]]))

(defn- transition
  "If a live tree is next to a burning tree, it burns;
otherwise, it catches fire with probability p1.
A burning tree dies.
A dead tree has probability p2 of being replaced by a live tree."
  [subgrid i j]
  (let [uc (get-in subgrid [i j])
        un (get-in subgrid [(dec i) j])
        us (get-in subgrid [(inc i) j])
        ue (get-in subgrid [i (inc j)])
        uw (get-in subgrid [i (dec j)])]
    (let [p1 0.01
          p2 0.3]
      (condp = uc
        :alive (cond
                 (some #{:burning} [un us ue uw]) :burning
                 (<= (Math/random) p1) :burning
                 :else :alive)
        :burning :dead
        (cond
          (<= (Math/random) p2) :alive
          :else :dead)))))

(defn simulate-forestfire
  [q m]
  (let [initial-values {:north-boundary :dead
                        :south-boundary :dead
                        :east-boundary :dead
                        :west-boundary :dead
                        :interior :alive}
        application {:initial-values initial-values
                     :transition transition}]
    (simulate q m application)))
