(ns quil-experiments.two.dynamic
  (:use [quil.core])
  (:require [quil.middleware :as m]))

(defn ratio->radians [num] (* num 2 (Math/PI)))

(defn ratio->sine [ratio] (Math/sin (ratio->radians ratio)))

(defn increment [previous-state steps]
  (mod (+ (/ 1 steps) previous-state)
       1))


(defn setup []
  (frame-rate 30)
  {:one 1
   :two 1})

(defn update [state]
  (let [slow-osc  (increment (:one state) 1500)
        fast-osc  (increment (:two state)  150)
        f-osc-255 (round (+ 100 (* 100 (ratio->sine two))))
        size (+ 280
                (* 125 (ratio->sine one))
                (* 25 (ratio->sine two)))]
    {:colour fast-osc-255
     :size   size}))

(defn draw [state]
  (background 240)
  (fill 0)
  (text (str (round (:size state))) 5 15)
  (text (str (:colour state)) 5 35)

  (no-stroke)
  (fill 0 (:colour state))
  (ellipse (/ (width) 2) (/ (height) 2) (:size state) (:size state)))
