module HW where

import Prelude hiding (map)

map :: (a -> b) -> [a] -> [b]

-- -- Incorrect: foldr doesn't work on infinite lists
-- map f = foldr (\x xs -> xs ++ [f x]) []

-- -- Incorrect: foldr, doesn't f all
-- map f = foldr (\x xs -> f x ++ xs) []

map f = foldl (\xs x -> f x : xs) []

map f = foldl (\xs x -> f x ++ [f xs]) []
