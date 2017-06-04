module Bob
  ( responseFor
  ) where

import Data.Char (isUpper, isAlpha, isSpace)

responseFor :: String -> String
responseFor xs
  | all isSpace xs = "Fine. Be that way!"
  | any isAlpha xs && all isUpperIfAlpha xs = "Whoa, chill out!"
  | last' (filter (not . isSpace) xs) == Just '?' = "Sure."
  | otherwise = "Whatever."
  where
    isUpperIfAlpha c = isUpper c || not (isAlpha c)
    last' [] = Nothing
    last' xs = Just (last xs)
