module Connect
  ( Game
  , Player(..)
  , newGame
  , columnSize
  , columnTokens
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array (index, replicate)
import Data.Int (round)

data Player
  = X
  | O

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


-- | Get the size of the columns in the game
columnSize :: Game -> Int
columnSize (Game { columnSize }) =
  columnSize


-- | Get the number of tokens in a column in the game.
columnTokens :: Game -> Int -> Maybe Int
columnTokens (Game { columns }) =
  index columns
