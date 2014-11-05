module AmpAmp where

import Prelude hiding ((&&))

-- We define it like this to prevent evaluation of the second param

(&&) :: Bool -> Bool -> Bool
(&&) True  x = x
(&&) False _ = False
