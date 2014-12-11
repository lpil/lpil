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
  {:slow-osc 0
   :fast-osc 0})

(defn update [state]
  (let [slow-osc  (increment (:slow-osc state) 1500)
        fast-osc  (increment (:fast-osc state)  150)
        f-osc-255 (round (+ 80 (* 80 (ratio->sine fast-osc))))
        size (+ 280
                (* 125 (ratio->sine slow-osc))
                (*  25 (ratio->sine fast-osc)))]
    {:slow-osc slow-osc
     :fast-osc fast-osc
     :colour   f-osc-255
     :size     size}))

(defn draw [state]
  (background 240)
  (fill 0)
  (text (str (round (:size state))) 5 15)
  (text (str (:colour state)) 5 35)

  (no-stroke)
  (fill 0 (- 255 (:colour state)))
  (ellipse (/ (width) 2) (/ (height) 2) (:size state) (:size state)))
