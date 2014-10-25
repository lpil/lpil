takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 _ [] = []
takeWhile2 pred (x:xs)
  | not $ pred x = []
  | otherwise    = x : takeWhile2 pred xs
