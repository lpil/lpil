module Length where

import Prelude hiding (length)

length :: Integral b => [a] -> b
length xs =
    let loop []     count = count
        loop (_:ys) count = loop ys $ succ count
    in loop xs 0

-- I think this builds up a lot of thunks and uses a lot of memory.
length2 :: Integral b => [a] -> b
length2 = foldr (\_ count -> succ count) 0
