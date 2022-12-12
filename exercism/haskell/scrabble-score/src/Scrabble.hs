module Scrabble
  ( scoreLetter
  , scoreWord
  ) where

import Data.Char (toLower)

scoreLetter :: Char -> Integer
scoreLetter = score . toLower
 where
  score 'a' = 1
  score 'e' = 1
  score 'i' = 1
  score 'o' = 1
  score 'u' = 1
  score 'l' = 1
  score 'n' = 1
  score 'r' = 1
  score 's' = 1
  score 't' = 1
  score 'd' = 2
  score 'g' = 2
  score 'b' = 3
  score 'c' = 3
  score 'm' = 3
  score 'p' = 3
  score 'f' = 4
  score 'h' = 4
  score 'v' = 4
  score 'w' = 4
  score 'y' = 4
  score 'k' = 5
  score 'j' = 8
  score 'x' = 8
  score 'q' = 10
  score 'z' = 10
  score _   = 0

scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter
