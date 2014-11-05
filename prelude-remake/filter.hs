module Filter where

import Prelude hiding (filter)

-- Explicit recursion
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs)
  | f x       = x : filter f xs
  | otherwise = filter f xs

-- Fold
filter1 :: (a -> Bool) -> [a] -> [a]
filter1 f = foldl fil []
  where
    fil acc x
      | f x       = x : acc
      | otherwise = acc
