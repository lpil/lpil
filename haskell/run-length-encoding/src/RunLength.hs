module RunLength
  ( decode
  , encode
  ) where

import Data.Char (isDigit)
import Data.List (group, take)

decode :: String -> String
decode "" = ""
decode [x] = [x]
decode chars@(first:rest)
  | (not . isDigit) first = first : decode rest
  | otherwise =
    let (number, char:rest) = span isDigit chars
        size = read number
    in replicate size char ++ decode rest

encode :: String -> String
encode = concatMap encodeRun . group
  where
    encodeRun :: String -> String
    encodeRun [] = []
    encodeRun [x] = [x]
    encodeRun xs = (show . length) xs ++ take 1 xs
