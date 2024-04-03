#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle

main = do
    dir <- pwd
    time <- datefile dir
    print time
