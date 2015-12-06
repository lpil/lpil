(ns overtone-scratch.core
  (:use [overtone.live]
        [overtone.inst.piano])
  (:gen-class))

(demo (sin-osc))

(demo 7 
      (pan2 (lpf (mix (saw [50 (line 100 1600 5) 101 100.5]))
                 (lin-lin (lf-tri (line 2 20 5)) -1 1 400 4000)) 
            0))

(defn piano-later [n]
  (at (+ (now) n) (piano 38)))

(piano-later 1000)

(map piano-later [0 1600 3200])
