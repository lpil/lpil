module Elem where

import Prelude hiding (elem)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem y (x:xs)
  | y == x    = True
  | otherwise = elem y xs

elem2 :: Eq a => a -> [a] -> Bool
elem2 x = any (== x)
