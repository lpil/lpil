module Main where

import System.Process
import Sound.SC3

main :: IO ()
main = do
    system "scsynth -u 57110 &"
    system "sleep 5"
    system "jack_connect SuperCollider:out_1 system:playback_1"
    system "jack_connect SuperCollider:out_2 system:playback_2"
    putStrLn "Bleep bloop"
