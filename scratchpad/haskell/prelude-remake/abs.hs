module Abs where

import Prelude hiding (abs)

abs :: Ord a => Num a => a -> a
abs x
  | x < 0     = -x
  | otherwise = x
