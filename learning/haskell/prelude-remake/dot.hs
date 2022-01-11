module Dot where

import Prelude hiding ((.))

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) x y z = x (y z)
