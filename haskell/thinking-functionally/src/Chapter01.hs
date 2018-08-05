module Chapter01 (song) where

import Data.List
import Data.Char

song :: Int -> String
song n
  | n < 1     = ""
  | otherwise = concat $ intersperse "\n" $ map verse [1..n]


verse :: Int -> String
verse n =
  line1 n ++
  "Went to mow a meadow\n" ++
  line3 n ++
  "Went to mow a meadow\n"


line1 :: Int -> String
line1 1 = "One man went to mow\n"
line1 n = capitalise $ toWord n ++ " men went to mow\n"


line3 :: Int -> String
line3 1 = "One man and his dog\n"
line3 n =
  let
    men 1 = "one man and his dog\n"
    men x = toWord x ++ " men"
  in
    capitalise .
    concat .
    intersperse ", " $
    map men [n, (n - 1)..1]


toWord :: Int -> String
toWord 1 = "one"
toWord 2 = "two"
toWord 3 = "three"
toWord 4 = "four"
toWord 5 = "five"
toWord 6 = "six"
toWord 7 = "seven"
toWord 8 = "eight"
toWord 9 = "nine"
toWord n = undefined

capitalise :: String -> String
capitalise (x:xs) = toUpper x : xs
capitalise ""     = ""
