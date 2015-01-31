module Fst where

import Prelude hiding (fst)

fst :: (a, b) -> a
fst (x, _) = x
