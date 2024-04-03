module Last where

import Prelude hiding (last)

last :: [a] -> a
last []     = error "last: empty list"
last [x]    = x
last (_:xs) = last xs
