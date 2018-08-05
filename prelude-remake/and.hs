module And where

import Prelude hiding (and)

and :: [Bool] -> Bool
and = foldr1 (&&)
