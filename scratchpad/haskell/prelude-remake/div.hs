module Div where

import Prelude hiding (div)

div :: (Num a, Ord a) => a -> a -> a
div x y = loop x y 0
  where
    loop n d acc
      | n < d     = acc
      | otherwise = loop (n - d) d (acc + 1)
