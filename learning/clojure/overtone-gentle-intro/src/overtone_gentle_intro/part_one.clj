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

; {
;   RLPF.ar(Dust.ar ([12,15]), LFNoise1.ar([0.3, 0.2]).range(100,3000), 0.02)
; }.play;
(demo 5 (rlpf (dust [12, 15])
              (lin-lin (lf-noise1 [0.3, 0.2]) -1 1 100 3000)
              0.02))

; We can record!
(recording-start "foo.wav")
(recording-stop)

; KICKDRUM
(def kick (sample (freesound-path 2086)))
