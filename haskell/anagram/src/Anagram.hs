module Anagram
  ( anagramsFor
  ) where

import Control.Arrow ((>>>))
import Data.Char (toLower)
import Data.List (nubBy, sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor word = filter isAnagram
 where
  isAnagram x = lowercase x /= lowercase word && toKey x == toKey word
  toKey     = lowercase >>> sort
  lowercase = map toLower
