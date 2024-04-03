module Repeat where

import Prelude hiding (repeat)

repeat :: a -> [a]
repeat x =
    let loop xs = x : loop xs
    in loop []
