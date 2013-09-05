(ns utils.helpers
  (:require [clojure.core.async :refer [<! >! chan go]]
             [fipp.edn :refer [pprint]]))

(defn pprinter []
  (let [in (chan)]
    (go (while true
          (let [value (<! in)]
            (if (nil? value)
              (println "")
              (pprint value)))))
    in))

(defn sink []
  (let [in (chan)]
    (go (while true
          (<! in)))
    in))

(defn print-all<!
  "Dump everything from channel to stdout until channel closes, then print a newline"
  [channel]
  (go
    (while true
      (let [value (<! channel)]
        (if (nil? value)
          (println "")
          (do
            (.print System/out (str value \newline \newline)) ; This is apparently different from (print value), which doesn't flush everything until the user hits return!
            (println ""))))))
  nil)

(defn bounded-buffer
  [producer consumer-request consumer bufsize]
  (go (loop [buf []]
        (let [alts (condp = (count buf)
                     ;; buffer full: accept only from consumer-request:
                     bufsize [consumer-request]
                     ;; buffer empty: accept only from producer:
                     0 [producer]
                     ;; otherwise accept from either:
                     [consumer-request producer])
              [value source] (alts! alts)]
          (condp = source
            consumer-request (do
                               (>! consumer (first buf))
                               (recur (vec (rest buf))))
            producer (recur (conj buf value))))))

    nil)

(defn buffered-chan
  [channel bufsize]
  (let [request (chan)
        consumer (chan)]
    (bounded-buffer channel request consumer bufsize)
    {:request request :response consumer}))