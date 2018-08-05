#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle

datePwd = do
    dir <- pwd
    datefile dir

main = do
    time <- datePwd
    print time
