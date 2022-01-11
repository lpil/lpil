#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle

main :: IO ExitCode
main = do
    mkdir "test"
    shell "tar czf test.tar.gz test" empty
