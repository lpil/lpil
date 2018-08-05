module Minimum where

import Prelude hiding (minimum)

minimum :: Ord a => [a] -> a
minimum = foldr1 min
