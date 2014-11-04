module Flip where

flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f a b = f b a
