module Null where

import Prelude hiding (null)

null :: [a] -> Bool
null [] = True
null _  = False
