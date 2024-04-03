module Product where

import Prelude hiding (product)

product :: Num a => [a] -> a
product []     = 1
product (x:xs) = product xs * x
