(ns cellular.laplace
  (:require [cellular.cellular :refer [simulate]]))

(defn- transitioner
  "In steady state, the temperature of every interior cell
is the average of the neighboring temperatures. This is the
discrete form of Laplace's equation.
The residual is a measure of how close the temperatures are to satisfying this equation.
The correction of a temperature is proportional to its residual.

The discrete Laplacian operator is equivalent to a convolution with the kernel:

   0  1  0
   1 -4  1
   0  1  0,

or, including the diagonals:

   0.5  1  0.5
    1  -6   1
   0.5  1  0.5.

For a large square grid relaxed in parity order, the relaxation factor
    fopt = 2 - 2*pi/n
ensures the fastest possible convergence towards stationary temperatures.
In numerical analysis, this method is called successive overrelaxation
with parity ordering."
  [n]
  (let [fopt (- 2 (/ (* 2 Math/PI) n))]
    (fn
      [subgrid i j]
      (let [uc (get-in subgrid [i j])
            un (get-in subgrid [(dec i) j])
            us (get-in subgrid [(inc i) j])
            ue (get-in subgrid [i (inc j)])
            uw (get-in subgrid [i (dec j)])
;            use (get-in subgrid [(inc i) (inc j)])
;            usw (get-in subgrid [(inc i) (dec j)])
;            unw (get-in subgrid [(dec i) (dec j)])
;            une (get-in subgrid [(dec i) (inc j)])
            ]
        (let [residual (- (/ (+ un us ue uw) 4.0) uc)]
;        For a good time try this instead:
;        (let [residual (- (/ (+ un us ue uw (/ (+ usw use unw une) 2.0)) 6.0) uc)]
;        Does it get all weird because the corners are not copied?
          (+ uc (* fopt residual)))))))

(defn simulate-laplace
  [q m]
  (let [initial-values {:north-boundary 0
                        :south-boundary 100
                        :east-boundary 100
                        :west-boundary 0
                        :interior 50}
        application {:initial-values initial-values
                     :transition (transitioner (* q m))}]
    (simulate q m application)))
