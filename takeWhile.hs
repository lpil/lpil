module TakeWhile where

takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 _ [] = []
takeWhile2 f (x:xs)
  | not $ f x = []
  | otherwise = x : takeWhile2 f xs
