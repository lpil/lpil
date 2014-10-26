-- Explicit recursion
filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2 f (x:xs)
  | f x       = x : filter2 f xs
  | otherwise = filter2 f xs

-- Fold
filter3 :: (a -> Bool) -> [a] -> [a]
filter3 f xs = foldl fil [] xs
  where fil acc x
          | f x       = x : acc
          | otherwise = acc
