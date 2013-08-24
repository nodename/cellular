(ns cellular.macros
  (:require [clojure.core.async :refer [<! >!]]))

(defmacro copy
  [count in out]
  `(dotimes [_# ~count]
    (>! ~out (<! ~in))))
       
(defmacro get-row
  [n in]
  `(loop [j# 0
         row# []]
    (if (= j# ~n)
      row#
      (recur (inc j#) (conj row# (<! ~in))))))