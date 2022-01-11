(ns quil-experiments.one.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [quil-experiments.one.dynamic :as dynamic]))

(q/defsketch quil-experiments
  :title "Hello, Quil!"
  :size [500 500]
  ; setup function called only once, during sketch initialization.
  :setup dynamic/setup
  ; update is called on each iteration before draw is called.
  ; It updates sketch state.
  :update dynamic/update
  :draw dynamic/draw
  ; This sketch uses functional-mode middleware.
  :middleware [m/fun-mode])
