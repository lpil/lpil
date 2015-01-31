module Hat where

import Prelude hiding ((^))

(^) :: (Num a, Integral b) => a -> b -> a
_ ^ 0 = 1
n ^ x
  | x < 0 = error "Negative exponent"
  | otherwise = n * n ^ (x - 1)
