module WordCount
  ( wordCount
  ) where

import Prelude (map, (/=), (+), (#))
import Data.Tuple (Tuple(..))
import Data.StrMap
import Data.Array
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Regex.Flags (noFlags, global)


wordCount :: String -> StrMap Int
wordCount text =
  text
  # String.toLower
  # Regex.replace (unsafeRegex "([\n,:!&$@^%.]|' | ')+" global) " "
  # Regex.split (unsafeRegex " +" noFlags)
  # filter (\x -> x /= "")
  # map (\word -> Tuple word 1)
  # fromFoldableWith (+)
