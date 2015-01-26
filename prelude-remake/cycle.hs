module Cycle where

import Prelude hiding (cycle)

cycle :: [a] -> [a]
cycle []     = []
cycle (x:xs) = x : cycle (xs ++ [x])
