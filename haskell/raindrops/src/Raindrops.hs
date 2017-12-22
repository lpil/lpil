module Raindrops
  ( convert
  ) where

import Data.Maybe (fromMaybe)

convert :: Int -> String
convert n = fromMaybe (show n)
  $ mconcat [sound 3 "Pling", sound 5 "Plang", sound 7 "Plong"]
 where
  sound div noise | mod n div == 0 = Just noise
                  | otherwise      = Nothing
