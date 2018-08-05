module Okc where

-- What is next in this series? 1, 4, 10, 19, 31, _

seq1 :: (Eq a, Num a) => a -> a
seq1 1 = 1
seq1 x = seq1 (x - 1) + (x - 1) * 3

-- print $ seq1 6
-- 46
