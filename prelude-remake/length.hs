module Length where

import Prelude hiding (length)

length :: Integral b => [a] -> b
length = foldl count 0
  where count acc _ = acc + 1
