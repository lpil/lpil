module Connect (Game, Player(..), newGame) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List (List)
import Data.Int (round)

data Player
  = X
  | Y

type Column =
  { elements :: List Player
  , maxSize :: Int
  }

newtype Game =
  Game
    { columns :: (Array Column)
    , currentPlayer :: Player
    }


newGame :: Player -> Int -> Int -> Maybe Game
newGame player numCols maxSize =
  if numCols < 1 then
    Nothing
  else if maxSize < 1 then
    Nothing
  else
    Just $ Game
      { columns: []
      , currentPlayer: player
      }
