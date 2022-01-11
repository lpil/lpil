module Connect
  ( Game
  , Player(..)
  , newGame
  , columnSize
  , getColumn
  , placeToken
  , currentPlayer
  , winner
  , draw
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (joinWith)
import Data.Foldable (findMap)
import Data.NonEmpty (NonEmpty)
import Data.Array
  ( reverse, index, replicate, updateAt, findIndex , length
  , fromFoldable, group, head, range
  )

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


type Grid =
  Array (Array (Maybe Player))

newtype Game =
  Game
    { columns :: Grid
    , currentPlayer :: Player
    }


-- | Constructs a new Connect N game
newGame :: Player -> Int -> Int -> Maybe Game
newGame startingPlayer numCols columnSize' = do
  guard $ numCols > 0
  guard $ columnSize' > 0
  pure $ Game
    { columns: replicate numCols (replicate columnSize' Nothing)
    , currentPlayer: startingPlayer
    }


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


-- | Construct a ASCII art version of the game
draw :: Game -> String
draw game@(Game { columns }) =
  (range 0 $ columnSize game - 1)
  # map drawRow
  # reverse
  # joinWith "\n"
    where
      drawRow n =
        map (flip index n >>> fromMaybe Nothing >>> showCell) columns
        # joinWith "|"
        # (\elems -> "|" <> elems <> "|")

      showCell :: Maybe Player -> String
      showCell Nothing = "_"
      showCell (Just p) = show p


winner :: Game -> Maybe Player
winner game =
  findMap (\f -> f game)
    [ verticalWinner
    , horizontalWinner
    ]
    where
      verticalWinner :: Game -> Maybe Player
      verticalWinner (Game { columns }) =
        findMap find4InARow columns

      horizontalWinner :: Game -> Maybe Player
      horizontalWinner game@(Game { columns }) =
        (range 0 $ columnSize game - 1)
        # findMap (\i -> map (safeIndex i) columns # find4InARow)

      find4InARow :: Array (Maybe Player) -> Maybe Player
      find4InARow =
        (group >>> findMap (fromFoldable >>> groupOfFour))

      safeIndex :: Int -> Array (Maybe Player) -> Maybe Player
      safeIndex i array =
        index array i
        # fromMaybe Nothing

      groupOfFour :: Array (Maybe Player) -> Maybe Player
      groupOfFour grp =
        let
          player =
            head grp # fromMaybe Nothing
        in
          if player /= Nothing && (length grp) == 4 then
            player
          else
            Nothing
