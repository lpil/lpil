(ns quil-experiments.two.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [quil-experiments.two.dynamic :as dynamic]))

(q/defsketch quil-experiments
  :title "Hello, Quil!"
  :size [500 500]
  :setup dynamic/setup
  :update dynamic/update
  :draw dynamic/draw
  :middleware [m/fun-mode])
