module Robot
  ( Bearing(East, North, South, West)
  , bearing
  , coordinates
  , mkRobot
  , simulate
  , turnLeft
  , turnRight
  ) where

import Data.Foldable (foldl)

data Bearing
  = North
  | East
  | South
  | West
  deriving (Eq, Show)

data Robot =
  Robot Bearing
        (Integer, Integer)

bearing :: Robot -> Bearing
bearing (Robot bearing _) = bearing

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ coordinates) = coordinates

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

simulate :: Robot -> String -> Robot
simulate = foldl execute
 where
  execute robot                'R' = right robot
  execute robot                'L' = left robot
  execute (Robot North (x, y)) 'A' = Robot North (x, y + 1)
  execute (Robot East  (x, y)) 'A' = Robot East (x + 1, y)
  execute (Robot South (x, y)) 'A' = Robot South (x, y - 1)
  execute (Robot West  (x, y)) 'A' = Robot West (x - 1, y)
  execute robot                _   = robot
  right (Robot b c) = Robot (turnRight b) c
  left (Robot b c) = Robot (turnLeft b) c

-- I think this should be Bearing -> Robot -> Robot
--
turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North

-- I think this should be Bearing -> Robot -> Robot
--
turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North
