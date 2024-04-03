;;; philandstuff's examples! Woo

(ns overtone-scratch.two
  (:use [overtone.live])
  (:gen-class))

(definst hat [volume 1.0]
  (let [osc (white-noise)
        env (env-gen (perc 0.001 0.2) :action FREE)]
    (* volume 1 osc env)))

(hat)

(defn beats [time]
  (at (+    0 time) (hat))
  (at (+  400 time) (hat))
  (at (+  800 time) (hat))
  (at (+ 1000 time) (hat))
  (at (+ 1200 time) (hat)))
(beats (now))

(defn loop-beats [time]
  (at (+    0 time) (hat))
  (at (+  400 time) (hat))
  (at (+  800 time) (hat))
  (at (+ 1000 time) (hat))
  (at (+ 1200 time) (hat))
  (let [next-time (+ time 1600)]
    (apply-at next-time loop-beats next-time [])))

(loop-beats (now))
(stop)

; Milliseconds are kinda confusing.
; Metronomes are kinda rad.

(defonce metro (metronome 180))

; Get the current beat number
(metro) ; Hey, it increments!

; Get the timestamp of beat N
(metro 4)
(metro 4.5)

; Lets do loop-beats again, but with the metro

(defn metro-beats [metro beat-num]
  (at (metro (+ beat-num 0))   (hat))
  (at (metro (+ beat-num 1))   (hat))
  (at (metro (+ beat-num 2))   (hat))
  (at (metro (+ beat-num 3))   (hat))
  (at (metro (+ beat-num 3.5)) (hat))
  (let [beat-next (+ beat-num 4)
        next-time (metro beat-num)]
    (apply-at next-time metro-beats metro beat-next [])))

(metro-beats metro (metro))

; We can alter the metronome's bpm

(metro :bpm 100)
(metro :bpm 200)
(metro :bpm 300)
(metro :bpm 400)
(metro :bpm 500)
(metro :bpm 600)
(metro :bpm 700)

(stop)
(hat)
