module Any where

import Prelude hiding (any)

any :: (a -> Bool) -> [a] -> Bool
any f [] = False
any f (x:xs)
  | f x       = True
  | otherwise = any f xs
