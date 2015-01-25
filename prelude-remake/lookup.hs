module Lookup where

import Prelude hiding (lookup)

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ []           = Nothing
lookup x ((y, z):rest)
  | x == y    = Just z
  | otherwise = lookup x rest
