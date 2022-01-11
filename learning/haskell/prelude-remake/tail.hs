module Tail where

import Prelude hiding (tail)

tail :: [a] -> [a]
tail []     = error "tail: empty list"
tail (_:xs) = xs
