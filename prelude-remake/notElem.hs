module NotElem where

import Prelude hiding (notElem)

notElem :: Eq a => a -> [a] -> Bool
notElem x = all (/= x)
