module Strain
  ( keep
  , discard
  ) where

discard :: (a -> Bool) -> [a] -> [a]
discard p = keep (not . p)

keep :: (a -> Bool) -> [a] -> [a]
keep p [] = []
keep p (x:xs) | p x       = x : keep p xs
              | otherwise = keep p xs
