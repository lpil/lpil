module DropWhile where

import Prelude hiding (dropWhile)

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []     = []
dropWhile f xxs@(x:xs)
  | f x = dropWhile f xs
  | otherwise = xxs
