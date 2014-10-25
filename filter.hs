filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2 f (x:xs)
  | f x       = x : filter2 f xs
  | otherwise = filter2 f xs
