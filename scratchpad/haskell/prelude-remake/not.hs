module Not where

import Prelude hiding (not)

not :: Bool -> Bool
not True  = False
not False = True
