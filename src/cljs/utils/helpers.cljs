(ns utils.helpers
  (:require [cljs.core.async :refer [<! chan]])
  (:require-macros [cljs.core.async.macros :refer [go alt!]]))

(defn sink []
  (let [in (chan)]
    (go (while true
          (<! in)))
    in))
