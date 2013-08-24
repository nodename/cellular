(ns utils.helpers
  (:require [clojure.core.async :refer [<! chan]]
            [clojure.pprint :refer [pprint]])
  (:require-macros [cljs.core.async.macros :refer [go alt!]]))

(defn pprinter []
  (let [in (chan)]
    (go (while true
          (pprint (<! in))))
    in))

(defn sink []
  (let [in (chan)]
    (go (while true
          (<! in)))
    in))
