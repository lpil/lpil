(ns overtone-gentle-intro.part-one
  (use overtone.live))

; A Gentle Introduction to Supercollider (Or rather, Overtone)
; Part One
; Basics

(println "Hello world!")

; An audio "Hello world!"
; { SinOsc.ar }.play;
(demo (sin-osc))

; { SinOsc.ar(LFNoise0.kr(10).range(500,1500), mul: 0.1) }.play
(demo 5 (* 0.5 (sin-osc (lin-lin (lf-noise0:kr 10)
                                 -1 1
                                 500 1500))))
