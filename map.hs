module Map where

-- Explicit recursion
map2 :: (a -> b) -> [a] -> [b]
map2 _ [] = []
map2 f (x:xs) = f x : map2 f xs

-- Foldl
map3 :: (a -> b) -> [a] -> [b]
map3 f xs = reverse $ foldl comp [] xs
  where
    comp acc x = f x : acc
