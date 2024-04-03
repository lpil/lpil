module Even where

import Prelude hiding (even)

even :: Integral a => a -> Bool
even x = mod x 2 == 0
