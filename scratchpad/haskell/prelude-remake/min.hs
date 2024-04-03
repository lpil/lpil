module Min where

import Prelude hiding (min)

min :: Ord a => a -> a -> a
min x y
  | x > y     = y
  | otherwise = x
