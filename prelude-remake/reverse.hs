module Reverse where

import Prelude hiding (reverse)

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []
