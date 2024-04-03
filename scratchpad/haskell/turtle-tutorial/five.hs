#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle

datePwd = do
    dir    <- pwd
    result <- datefile dir
    return result

main = do
    time <- datePwd
    print time
