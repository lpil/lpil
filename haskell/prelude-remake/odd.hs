module Odd where

import Prelude hiding (odd)

odd :: Integral a => a -> Bool
odd = not . even
