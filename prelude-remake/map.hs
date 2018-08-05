module Map where

import Prelude hiding (map)

-- Explicit recursion
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

-- Foldl
map1 :: (a -> b) -> [a] -> [b]
map1 f xs = reverse $ foldl comp [] xs
  where
    comp acc x = f x : acc
