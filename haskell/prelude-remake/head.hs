module Head where

import Prelude hiding (head)

head :: [a] -> a
head []    = error "Prelude.head: empty list"
head (x:_) = x
