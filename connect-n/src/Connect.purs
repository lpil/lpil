module Connect
  ( Game
  , Player(..)
  , newGame
  , columnSize
  , getColumn
  , placeToken
  , currentPlayer
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Array (index, replicate, updateAt, findIndex, length)

data Player
  = X
  | O

instance eqPlayer :: Eq Player where
  eq X X = true
  eq O O = true
  eq _ _ = false

instance showPlayer :: Show Player where
  show X = "X"
  show O = "O"


newtype Game =
  Game
    { columns :: (Array (Array (Maybe Player)))
    , currentPlayer :: Player
    }


-- | Constructs a new Connect N game
newGame :: Player -> Int -> Int -> Maybe Game
newGame startingPlayer numCols columnSize' = do
  _ <- assertPositive numCols
  _ <- assertPositive columnSize'
  pure $ Game
    { columns: replicate numCols (replicate columnSize' Nothing)
    , currentPlayer: startingPlayer
    }

  where
    assertPositive n =
      if n > 0 then Just n else Nothing


-- | Get the size of the columns in the game.
columnSize :: Game -> Int
columnSize (Game { columns }) =
  index columns 0
  # map length
  # fromMaybe 1


-- | Get the number of tokens in a column in the game.
getColumn :: Game -> Int -> Maybe (Array (Maybe Player))
getColumn (Game { columns }) =
  index columns


-- | Get the player who is to take this turn.
currentPlayer :: Game -> Player
currentPlayer (Game game) =
  game.currentPlayer


-- | Get the Player to take the next turn
nextPlayer :: Game -> Player
nextPlayer (Game { currentPlayer: X }) = O
nextPlayer (Game { currentPlayer: O }) = X


-- | Place a token in a column. Fails if column is full or out
-- | of bounds.
placeToken :: Game -> Int -> Maybe Game
placeToken game@(Game game') n = do
  column <- getColumn game n
  cellIndex <- findIndex isNothing column
  newColumn <- updateAt cellIndex (Just $ currentPlayer game) column
  columns <- updateAt n newColumn game'.columns
  Just $ Game $ game' { columns = columns
                      , currentPlayer = nextPlayer game }
