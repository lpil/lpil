module ZipWith where

import Prelude hiding (zipWith)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _          = []
zipWith _ _ []          = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
