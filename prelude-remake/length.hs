module Length where

import Prelude hiding (length)

length :: Integral b => [a] -> b
length = foldr (\_ count -> succ count) 0
