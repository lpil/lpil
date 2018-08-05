module Or where

import Prelude hiding (or)

or :: [Bool] -> Bool
or = foldr1 (||)
