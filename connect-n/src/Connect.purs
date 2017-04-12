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
  assertPositive numCols
  >>= buildCols
  >>= buildGame

  where
    assertPositive n =
      if n > 0 then Just n else Nothing

    newColumn maxSize =
      if maxSize < 1 then
        Nothing
      else
        Just { elements: Nil, maxSize }

    buildCols n =
      sequence $ replicate n (newColumn maxSize)

    buildGame columns =
      Just $ Game { columns, currentPlayer }


