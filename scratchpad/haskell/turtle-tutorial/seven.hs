#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle

datePwd :: IO UTCTime
datePwd = do
    dir <- pwd
    datefile dir

main :: IO ()
main = do
    timeVal <- datePwd
    print timeVal
