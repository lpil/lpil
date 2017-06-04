module DNA
  ( nucleotideCounts
  ) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs =
  let initial = Map.fromList [('A', 0), ('C', 0), ('G', 0), ('T', 0)]
  in count initial xs
  where
    count map [] = Right map
    count map (x:xs)
      | valid x = count (increment x map) xs
      | otherwise = Left (x : " is not a valid base")
    valid 'A' = True
    valid 'C' = True
    valid 'G' = True
    valid 'T' = True
    valid _ = False
    increment x = Map.insertWith (+) x 1
