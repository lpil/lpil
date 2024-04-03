module Iterate where

import Prelude hiding (iterate)

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)
