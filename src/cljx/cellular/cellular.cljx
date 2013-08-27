(ns cellular.cellular
#+clj   (:require [clojure.core.async :refer [<! >! chan go]])
#+cljs  (:require [cljs.core.async :refer [>! <! chan]])
#+cljs  (:require-macros [cljs.core.async.macros :refer [go alt!]]))

(defn initializer
  [n initial-values]
  (let [{:keys [north-boundary south-boundary east-boundary west-boundary interior]} initial-values]
    (fn [i j]
      (cond
        (zero? i) north-boundary
        (= (inc n) i) south-boundary
        (= (inc n) j) east-boundary
        (zero? j) west-boundary
        :else interior))))

(defn newgrid-row
  [m initialize i0 j0 i]
  (let [row (atom [])]
    (doseq [j (range (+ 2 m))]
      (swap! row conj (initialize (+ i0 i) (+ j0 j))))
    row))
      
(defn newgrid
  [m initialize]
  (fn [qi qj]
    (let [i0 (* (dec qi) m)
          j0 (* (dec qj) m)
          grid (atom [])]
      (doseq [i (range (+ 2 m))]
        (swap! grid conj @(newgrid-row m initialize i0 j0 i)))
      grid)))
        
(defn phase1-step
  [q m qi qj channels subgrid-atom k]
  (let [{:keys [north south east west]} channels
        done (chan)
        out (chan)]
    (go
      (when (> qi 1) (swap! subgrid-atom assoc-in [0 k] (<! north)))
      (>! done :done))
    (go
      (when (< qi q) (>! south ((@subgrid-atom m) k)))
      (>! done :done))
    (go
      (when (< qj q) (>! east ((@subgrid-atom k) m)))
      (>! done :done))
    (go
      (when (> qj 1) (swap! subgrid-atom assoc-in [k 0] (<! west)))
      (>! done :done))
    (go
      (dotimes [_ 4]
        (<! done))
      (>! out subgrid-atom))
    out))

(defn exchange-phase1
  [q m qi qj parity channels subgrid-atom]
  ;; qi row number, qj column number
  ;; qi, qj go from 1 to q inclusive
  (let [out (chan)
        last (- m parity)]
    (go
      (let [new-subgrid-atom (loop [k (- 2 parity)
                         subgrid-atom subgrid-atom]
                    (if (> k last)
                      subgrid-atom
                      (recur (+ 2 k) (<! (phase1-step q m qi qj channels subgrid-atom k)))))]
        (>! out new-subgrid-atom)))
    out))

(defn phase2-step
  [q m qi qj channels subgrid-atom k]
  (let [{:keys [north south east west]} channels
        done (chan)
        out (chan)]
    (go
      (when (> qi 1) (>! north ((@subgrid-atom 1) k)))
      (>! done :done))
    (go
      (when (< qi q) (swap! subgrid-atom assoc-in [(inc m) k] (<! south)))
      (>! done :done))
    (go
      (when (< qj q) (swap! subgrid-atom assoc-in [k (inc m)] (<! east)))
      (>! done :done))
    (go
      (when (> qj 1) (>! west ((@subgrid-atom k) 1)))
      (>! done :done))
    (go
      (dotimes [_ 4]
        (<! done))
      (>! out subgrid-atom))
    out))

(defn exchange-phase2
  [q m qi qj parity channels subgrid-atom]
  (let [out (chan)
        last (dec (+ m parity))]
    (go
      (let [new-subgrid-atom (loop [k (inc parity)
                         subgrid-atom subgrid-atom]
                    (if (> k last)
                      subgrid-atom
                      (recur (+ 2 k) (<! (phase2-step q m qi qj channels subgrid-atom k)))))]
        (>! out new-subgrid-atom)))
    out))

(defn exchange
  [q m qi qj parity channels subgrid-atom]
  (let [out (chan)]
    (go
      (let [subgrid-atom (<! (exchange-phase1 q m qi qj parity channels subgrid-atom))
            subgrid-atom (<! (exchange-phase2 q m qi qj parity channels subgrid-atom))]
        (>! out subgrid-atom)))
    out))

(defn relax-phase
  [transition q m qi qj channels]
  (fn [subgrid-atom parity]
    (let [assoc-next-states-in (fn [subgrid-atom]
                                 (doseq [i (range 1 (inc m))]
                                   (let [k (mod (+ i parity) 2)
                                         last (- m k)]
                                     (doseq [j (range (- 2 k) (inc last) 2)]
                                       (swap! subgrid-atom assoc-in [i j] (transition @subgrid-atom i j)))))
                                 subgrid-atom)]
      
      (let [out (chan)]
        (go
          (let [subgrid-atom (<! (exchange q m qi qj (- 1 parity) channels subgrid-atom))
                subgrid-atom (assoc-next-states-in subgrid-atom)]
            (>! out subgrid-atom)))
        out))))
      
(defn relaxation-step
  [transition q m qi qj channels subgrid-atom]
  (let [out (chan)]
    (go
      (let [relaxation-phase (relax-phase transition q m qi qj channels)
            subgrid-atom (<! (relaxation-phase subgrid-atom 0))
            subgrid-atom (<! (relaxation-phase subgrid-atom 1))]
        (>! out subgrid-atom)))
    out))

(defn relaxation
  [q m transition]
  (fn [qi qj channels subgrid-atom steps]
    (let [out (chan)]
      (go
        (let [subgrid-atom (loop [step 0
                       subgrid-atom subgrid-atom]
                  (if (= step steps)
                    subgrid-atom
                    (recur (inc step) (<! (relaxation-step transition q m qi qj channels subgrid-atom)))))]
          (>! out subgrid-atom)))
      out)))

(defn outputter
  [q m]
  (fn [qi qj in out]
    (let [subgrid-in (chan)
          copy (fn [count in out]
                 (go
                   (dotimes [_ count]
                     (>! out (<! in)))))]
      (go
        (while true
            (let [subgrid (<! subgrid-in)]
              (dotimes [i m]
                (let [ii (inc i)]
                  (dotimes [j m]
                    (let [jj (inc j)]
                      (>! out ((subgrid ii) jj))))
                  (copy (* (- q qj) m) in out)))
              (copy (* (- q qi) m m q) in out))))
      subgrid-in)))

(def RELAXATION-STEPS-PER-OUTPUT 1)
  
(defn node
  [init relax steps output]
  (fn [qi qj channels]
  ;; qi row number; qj column number
    (let [{:keys [data-in data-out]} channels
          out (output qi qj data-in data-out)]
      (go
        (loop [step 0
               subgrid-atom (init qi qj)]
          (>! out @subgrid-atom)
          (when (< step steps)
            (recur (+ RELAXATION-STEPS-PER-OUTPUT step) (<! (relax qi qj channels subgrid-atom RELAXATION-STEPS-PER-OUTPUT)))))))))

(defn master
  "Input the grid of nXn values (states) from the processors, one element at a time.
The single input channel comes from the output function of channel h0q (the last channel of
the north boundary row, i.e. the channel that receives the output from the pipeline threaded
through the interior elements only."
  [n in start-time]
  (let [out (chan)
        get-row (fn [n in]
                  (let [out (chan)]
                    (go
                      (loop [row []]
                        (if (= (count row) n)
                          (>! out row)
                          (recur (conj row (<! in))))))
                    out))]
    (go
      (while true
        (let [grid (loop [grid []]
                     (if (= (count grid) n)
                       grid
                       (recur (conj grid (<! (get-row n in))))))
              elapsed-ms #+clj (long (/ (- (System/nanoTime) start-time) 1000000)) #+cljs (- (.getTime (js/Date.)) start-time)]
          (>! out {:elapsed-ms elapsed-ms :grid grid}))))
    out))

(defn simulate
"Create a matrix of qXq processor nodes.
Every node is connected to its nearest neighbors (if any)
by four communication channels named north, south, east, and west.
Each processor node is responsible for a subgrid of mXm data cells
within the complete nXn grid, where n = q * m
(m must be even because we did not bother to code for the odd case.)
After initializing its subgrid, each node will update the subgrid
a fixed number of times (specified by the steps parameter)
before outputting the final values to the out channel.
The nodes will update their subgrids simultaneously.
In numerical analysis, grid iteration is known as relaxation.

The nXn data grid is surrounded by a row of boundary cells on each side.
The application object must specify:
    a fixed value for the elements of each boundary
    and an initial value for the interior elements;
    and a transition function that returns the next value for a cell
    given the subgrid and the cell's position in the subgrid.
"
  [q m steps application]
  (let [{:keys [initial-values transition]} application
        chan-row #(vec (repeatedly (inc q) chan)) ;; we only use elements 1 through q of chan-row
        chan-matrix #(vec (repeatedly (inc q) chan-row))
        ew-channels (chan-matrix)
        ns-channels (chan-matrix)
        output-channels (chan-matrix)
        n (* q m)
        initialize (initializer n initial-values)
        init (newgrid m initialize)
        relax (relaxation q m transition)
        output (outputter q m)
        init-node (node init relax steps output)
        start-time #+clj (System/nanoTime) #+cljs (.getTime (js/Date.))
        out (master n ((output-channels 0) q) start-time)]
    
    ;; node coordinates range from 1 to q inclusive
    
    (doseq [i (range 1 (inc q))]
      (let [channels {:north ((ns-channels (dec i)) 1)
                      :south ((ns-channels i) 1)
                      :east ((ew-channels i) 1)
                      :west ((ew-channels (dec i)) q)
                      :data-in ((output-channels i) 1)
                      :data-out ((output-channels (dec i)) q)
                      }]
        (init-node i 1 channels)))
    
    (doseq [i (range 1 (inc q))
            j (range 2 (inc q))]
      (let [channels {:north ((ns-channels (dec i)) j)
                      :south ((ns-channels i) j)
                      :east ((ew-channels i) j)
                      :west ((ew-channels i) (dec j))
                      :data-in ((output-channels i) j)
                      :data-out ((output-channels i) (dec j))
                      }]
        (init-node i j channels)))
    
    out))
