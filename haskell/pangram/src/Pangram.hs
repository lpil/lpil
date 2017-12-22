module Pangram
  ( isPangram
  ) where

import Data.Char (toLower)
import Data.Set (fromList, isSubsetOf)

isPangram :: String -> Bool
isPangram text = isSubsetOf alphabet (lowerSet text)
 where
  lowerSet = fromList . toLower'
  toLower' = map toLower
  alphabet = fromList "abcdefghijklmnopqrstuvwxyz"
