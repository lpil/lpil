(ns quil-experiments.two.dynamic
  (:use [quil.core])
  (:require [quil.middleware :as m]))

(defn cycles [num] (* num 2 (. Math PI)))

(defn increment [previous-state steps]
  (mod (+ (/ 1 steps) previous-state)
       1))

(defn setup []
  (frame-rate 30)
  {:one 1
   :two 1})

(defn update [state]
  (let [one (increment (:one state) 1000)
        two (increment (:two state)  100)
        size (+ 300
                (* 90 (Math/sin (cycles one)))
                (* 10 (Math/sin (cycles two))))]
    {:one one
     :two two
     :size size}))

(defn draw [state]
  (background 240)
  (text (str (round (current-frame-rate))) 5 15)
  (fill 0)
  (ellipse (/ (width) 2) (/ (height) 2) (:size state) (:size state)))
