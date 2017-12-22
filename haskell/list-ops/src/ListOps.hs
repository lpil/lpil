module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Control.Arrow ((>>>))
import Prelude
       hiding ((++), concat, filter, foldr, length, map, reverse)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (x:xs) =
  let nextAcc = f acc x
      result  = foldl' f nextAcc xs
  in  seq nextAcc result -- How do I make this strict with bang patterns?

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc = reverse >>> foldl' (flip f) acc

length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs

reverse :: [a] -> [a]
reverse = go []
 where
  go acc []     = acc
  go acc (x:xs) = go (x : acc) xs

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs) = if p x then x : filter p xs else filter p xs

(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : (xs ++ ys)

concat :: [[a]] -> [a]
concat []           = []
concat ([]    :xss) = concat xss
concat ((x:xs):xss) = x : concat (xs : xss)
