module Connect
  ( Game
  , Player(..)
  , newGame
  , columnSize
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array (replicate)
import Data.Traversable (sequence)
import Data.Int (round)

data Player
  = X
  | Y

newtype Game =
  Game
    { columns :: (Array Int)
    , currentPlayer :: Player
    , columnSize :: Int
    }


-- | Constructs a new Connect N game
newGame :: Player -> Int -> Int -> Maybe Game
newGame currentPlayer numCols columnSize = do
  _ <- assertPositive numCols
  _ <- assertPositive columnSize
  pure $ Game
    { columns: replicate numCols 0
    , columnSize
    , currentPlayer
    }

  where
    assertPositive n =
      if n > 0 then Just n else Nothing


columnSize :: Game -> Int
columnSize (Game { columnSize }) =
  columnSize
