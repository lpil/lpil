module SumOfMultiples
  ( sumOfMultiples
  ) where

import Control.Category ((>>>))
import Data.Foldable (sum)
import qualified Data.Set

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  (concatMap validMultiples >>> Data.Set.fromList >>> sum) factors
 where
  multiples x = map (x *) [1 ..]
  validMultiples = takeWhile (< limit) . multiples
