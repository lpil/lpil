module Init where

import Prelude hiding (init)

init :: [a] -> [a]
init []     = error "init: empty list"
init [_]    = []
init (x:xs) = x : init xs
