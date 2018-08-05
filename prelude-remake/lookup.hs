module Lookup where

import Prelude hiding (lookup)

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup x ((key, val):rest)
  | x == key  = Just val
  | otherwise = lookup x rest
