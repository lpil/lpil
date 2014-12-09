(ns quil-experiments.one.dynamic
  (:use [quil.core])
  (:require [quil.middleware :as m]))


(defn setup []
  (frame-rate 30)

  ; setup function returns initial state.
  {})

(defn update [state]
  {})

(defn draw [state]
  (fill 255 10)
  (rect -1 -1 (+ 1 (width)) (+ 1 (height)))

  (if (mouse-pressed?)
    (do (line 0       0        (mouse-x) (mouse-y))
        (line 0       (height) (mouse-x) (mouse-y))
        (line (width) 0        (mouse-x) (mouse-y))
        (line (width) (height) (mouse-x) (mouse-y)))))
