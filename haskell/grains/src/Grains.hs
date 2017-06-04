module Grains
  ( square
  , total
  ) where

import qualified Data.List as List

squares :: [Integer]
squares = 1 : iterate (* 2) 1

square :: Integer -> Maybe Integer
square n
  | n > 64 = Nothing
  | n > 0 = Just (List.genericIndex squares n)
  | otherwise = Nothing

total :: Integer
total = (sum . take 65) squares - 1
