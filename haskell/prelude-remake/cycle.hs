module Cycle where

import Prelude hiding (cycle)

cycle :: [a] -> [a]
cycle [] = error "Prelude.cycle: empty list"
cycle xs = loop where loop = xs ++ loop
