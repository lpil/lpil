module Flip where

import Prelude hiding (flip)

flip :: (a -> b -> c) -> b -> a -> c
flip f a b = f b a
