module HW where

import Prelude hiding ((^))

(^) :: (Num a, Integral b) => a -> b -> a

-- _ ^ 0 = 0
-- m ^ n = m * m ^ (n - 1)

-- correct
-- _ ^ 0 = 1
-- m ^ n = m * m ^ (n - 1)

-- _ ^ 0 = 1
-- m ^ n = m * m ^ n - 1

-- _ ^ 0 = 1
-- m ^ n = n * n ^ (m - 1)

-- correct
_ ^ 0 = 1
m ^ n = m * (^) m (n - 1)

-- _ ^ 0 = 1
-- m ^ n = m * m * m ^ (n - 2)

-- _ ^ 0 = 1
-- m ^ n = (m * m) ^ (n - 1)

-- m ^ 1 = m
-- m ^ n = m * m ^ (n - 1)
