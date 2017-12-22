module ETL
  ( transform
  ) where

import Control.Category ((>>>))
import Data.Char (toLower)
import Data.Map (Map, toList, fromList)

transform :: Map a String -> Map Char a
transform = toList >>> concatMap transformPair >>> fromList
 where
  transformPair (score, letters) = map (tuple score) letters
  tuple score letter = (toLower letter, score)
