module Garden
  ( Plant(..)
  , defaultGarden
  , garden
  , lookupPlants
  ) where

import Data.List (sort)
import Data.List.Split (chunksOf)
import Data.Map (Map, (!), fromListWith)
import Data.Maybe (mapMaybe)

data Plant
  = Clover
  | Grass
  | Radishes
  | Violets
  deriving (Eq, Show)

defaultGarden :: String -> Map String [Plant]
defaultGarden = garden
  [ "Alice"
  , "Bob"
  , "Charlie"
  , "David"
  , "Eve"
  , "Fred"
  , "Ginny"
  , "Harriet"
  , "Ileana"
  , "Joseph"
  , "Kincaid"
  , "Larry"
  ]

garden :: [String] -> String -> Map String [Plant]
garden students plants =
  fromListWith (flip (++))
    $   concatMap assignRow
    $   convertPlantRow
    <$> lines plants
 where
  assignRow rowPlants = zip (sort students) (chunksOf 2 rowPlants)
  convertPlantRow = mapMaybe convertPlant
  convertPlant 'C' = Just Clover
  convertPlant 'G' = Just Grass
  convertPlant 'R' = Just Radishes
  convertPlant 'V' = Just Violets
  convertPlant _   = Nothing

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants student garden = garden ! student
