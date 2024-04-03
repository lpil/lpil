module Drop where

import Prelude hiding (drop)

drop :: Int -> [a] -> [a]
drop _ []     = []
drop 0 xs     = xs
drop n (_:xs) = drop (n - 1) xs
