module Hamming
  ( distance
  ) where

distance :: String -> String -> Maybe Int
distance = distance' 0
  where
    distance' count "" "" = Just count
    distance' _ _ "" = Nothing
    distance' _ "" _ = Nothing
    distance' count (x:xs) (y:ys)
      | x == y = distance' count xs ys
      | otherwise = distance' (count + 1) xs ys
