module FizzBuzz where

fizzbuzz :: (Show a, Integral a) => a -> String
fizzbuzz x = unlines $ map fb [1..x]
  where 
    fb y
      | mod y 15 == 0 = "FizzBuzz"
      | mod y  5 == 0 = "Fizz"
      | mod y  3 == 0 = "Buzz"
      | otherwise     = show x
