module Negate where

import Prelude hiding (negate)

negate :: Num a => a -> a
negate = (0 -)
