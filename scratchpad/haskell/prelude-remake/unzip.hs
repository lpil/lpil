module Unzip where

import Prelude hiding (unzip)

unzip :: [(a, b)] -> ([a], [b])
unzip xs =
    let split (y, z) (ys, zs) = (y:ys, z:zs)
    in foldr split ([], []) xs
