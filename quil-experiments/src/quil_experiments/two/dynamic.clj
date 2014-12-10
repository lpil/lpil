(ns quil-experiments.two.dynamic
  (:use [quil.core])
  (:require [quil.middleware :as m]))

(defn cycles [num] (* num 2 (Math/PI)))

(defn increment [previous-state steps]
  (mod (+ (/ 1 steps) previous-state)
       1))

(defn setup []
  (frame-rate 30)
  {:one 1
   :two 1})

(defn update [state]
  (let [one (increment (:one state) 1500)
        two (increment (:two state)  150)
        size (+ 280
                (* 125 (Math/sin (cycles one)))
                (* 25 (Math/sin (cycles two))))]
    {:one one
     :two two
     :size size}))

(defn draw [state]
  (background 240)
  (text (str (round (:size state))) 5 15)
  (fill 0)
  (ellipse (/ (width) 2) (/ (height) 2) (:size state) (:size state)))
