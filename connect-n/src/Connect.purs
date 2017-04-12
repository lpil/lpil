module Connect (Game, Player(..), newGame) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List (List(..))
import Data.Array (replicate)
import Data.Traversable (sequence)
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
newGame currentPlayer numCols maxSize =
  if numCols < 1 then
    Nothing
  else
    replicate numCols (newColumn maxSize)
    # sequence
    >>= buildGame

  where
    buildGame columns =
      Just $ Game { columns, currentPlayer }

newColumn :: Int -> Maybe Column
newColumn maxSize =
  if maxSize < 1 then
    Nothing
  else
    Just { elements: Nil, maxSize }
