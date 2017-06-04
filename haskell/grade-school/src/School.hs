module School
  ( School
  , add
  , empty
  , grade
  , sorted
  ) where

import Control.Arrow (second)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

newtype School =
  School (Map Int [String])

add :: Int -> String -> School -> School
add gradeNum student (School school) =
  School $ Map.insertWith (++) gradeNum [student] school

empty :: School
empty = School Map.empty

grade :: Int -> School -> [String]
grade gradeNum (School school) = sort . fromMaybe [] . Map.lookup gradeNum $ school

sorted :: School -> [(Int, [String])]
sorted (School school) = map (second sort) . Map.toList $ school
