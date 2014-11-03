module Zip where

zip1 :: [a] -> [b] -> [(a, b)]
zip1 _      []     = []
zip1 []     _      = []
zip1 (x:xs) (y:ys) = (x, y) : zip1 xs ys
