module Zip where

import Prelude hiding (zip)

zip :: [a] -> [b] -> [(a, b)]
zip _      []     = []
zip []     _      = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys
