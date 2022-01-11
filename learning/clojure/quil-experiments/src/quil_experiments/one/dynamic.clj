(ns quil-experiments.one.dynamic
  (:require [quil.core :as q]
            [quil.middleware :as m]))


(defn setup []
  (q/frame-rate 30)

  ; setup function returns initial state.
  {})

(defn update [state]
  {})

(defn draw [state]
  (q/fill 255 10)
  (q/rect -1 -1 (inc (q/width)) (inc (q/height)))

  (when (q/mouse-pressed?)
    (q/line 0         0          (q/mouse-x) (q/mouse-y))
    (q/line 0         (q/height) (q/mouse-x) (q/mouse-y))
    (q/line (q/width) 0          (q/mouse-x) (q/mouse-y))
    (q/line (q/width) (q/height) (q/mouse-x) (q/mouse-y))))
