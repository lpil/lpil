(ns quil-experiments.two.dynamic
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn ratio->radians [num] (* num 2 (Math/PI)))

(defn ratio->sine [ratio] (Math/sin (ratio->radians ratio)))

(defn increment [previous-state steps]
  (mod (+ (/ 1 steps) previous-state)
       1))


(defn calc-f-osc-255 [fast-osc]
  (round (+ 80 (* 80 (ratio->sine fast-osc)))))

(defn calc-size [slow-osc fast-osc]
  (+ 280
     (* 125 (ratio->sine slow-osc))
     (*  25 (ratio->sine fast-osc))))


(defn setup []
  (q/frame-rate 30)
  {:slow-osc 0
   :fast-osc 0
   :colour   0
   :size     280})

(defn update [state]
  (let [slow-osc  (increment (:slow-osc state) 1500)
        fast-osc  (increment (:fast-osc state)  150)]

    {:slow-osc slow-osc
     :fast-osc fast-osc
     :colour   (calc-f-osc-255 fast-osc)
     :size     (calc-size slow-osc fast-osc)}))

(defn draw [state]
  (q/background 240)
  (q/fill 0)
  (q/text (str (round (:size state))) 5 15)
  (q/text (str (:colour state)) 5 35)

  (q/no-stroke)
  (q/fill 0 (- 255 (:colour state)))
  (q/ellipse (/ (q/width) 2) (/ (q/height) 2) (:size state) (:size state)))
