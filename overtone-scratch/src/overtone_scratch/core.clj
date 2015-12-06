(ns overtone-scratch.core
  (:use [overtone.live]
        [overtone.inst.piano])
  (:gen-class))

(demo (sin-osc))

(defn piano-later [n]
  (at (+ (now) n) (piano 38)))

(defn play-chord [a-chord]
  (doseq [note a-chord] (piano note)))

(piano-later 1000)

(map piano-later [0 1600 3200])

(play-chord (chord :c4 :minor))
(play-chord (chord :d4 :minor))

(let [time (now)]
  (at (+ time 0000) (play-chord (chord :f4 :minor)))
  (at (+ time 1000) (play-chord (chord :e4 :minor)))
  (at (+ time 3000) (play-chord (chord :d4 :minor)))
  (at (+ time 4000) (play-chord (chord :c4 :minor))))

(defn play [time notes time-sep]
  (let [note (first notes)
        next-time (+ time time-sep)]
    (piano (- note 12))
    (apply-at next-time play [next-time (rest notes) time-sep])))

(defn play [time notes time-sep])

(def my-scale 
  (reverse (scale :Bb4 :minor)))

(let [time (now)]
  (play (now) (cycle my-scale) 280)
  (play (now) (cycle my-scale) 285))

(stop)
