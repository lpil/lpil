module Acronym
  ( abbreviate
  ) where

import Data.Char (toUpper, isUpper)

abbreviate :: String -> String
abbreviate "" = ""
abbreviate (c1:c2:rest)
  | isUpper c1 && isUpper c2 = abbreviate (c1 : rest)
  | isSpace c1 = toUpper c2 : abbreviate rest
  where
    isSpace ' ' = True
    isSpace '-' = True
    isSpace _ = False
abbreviate (c:rest)
  | isUpper c = c : abbreviate rest
  | otherwise = abbreviate rest
