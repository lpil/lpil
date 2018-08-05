module TakeWhile where

import Prelude hiding (takeWhile)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x:xs)
  | not $ f x = []
  | otherwise = x : takeWhile f xs
