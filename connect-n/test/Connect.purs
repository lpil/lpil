module Test.Connect (tests) where

import Prelude
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Data.Maybe (Maybe(..), isJust, isNothing)
import Connect
  ( Player(..)
  , newGame
  , columnSize
  , getColumn
  , placeToken
  , currentPlayer
  , draw
  )

tests :: forall e. TestSuite e
tests = do
  suite "Connect.newGame" do
    test "Just Game with > 0 board size ints" do
      Assert.assert "should be Just Game" $
        isJust (newGame X 1 1)

    test "Nothing with <= 0 numCols" do
      Assert.assert "should be Nothing" $
        isNothing (newGame X 0 1)

    test "Nothing with <= 0 maxSize" do
      Assert.assert "should be Nothing" $
        isNothing (newGame X 1 0)

  suite "Connect.columnSize" do
    test "is the same as the columnSize the game was constructed with" do
      Assert.equal (Just 5)
        (newGame X 1 5 # map columnSize)

  suite "Connect.currentPlayer" do
    test "gets the current player" do
      Assert.equal (Just X) $
        (newGame X 1 1 # map currentPlayer)

      Assert.equal (Just O) $
        (newGame O 1 1 # map currentPlayer)

  suite "Connect.getColumn" do
    test "columns start with no tokens" do
      Assert.equal (Just [Nothing, Nothing, Nothing])
        (newGame X 2 3 >>= flip getColumn 0)

      Assert.equal (Just [Nothing, Nothing, Nothing])
        (newGame O 2 3 >>= flip getColumn 1)

    test "Nothing for out of bounds columns" do
      Assert.equal Nothing
        (newGame O 2 5 >>= flip getColumn 2)

  suite "Connect.placeToken" do
    test "fails if column out of bounds" do
      Assert.assert "should be Nothing" $
        isNothing (newGame X 1 1 >>= flip placeToken 1)

    test "fails if column is full" do
      Assert.assert "should be Nothing" $
        isNothing $
          newGame X 1 1
          >>= flip placeToken 0
          >>= flip placeToken 0

    test "increments the column" do
      Assert.equal (Just [Just X]) $
        newGame X 1 1
        >>= flip placeToken 0
        >>= flip getColumn 0

    test "sets the next player" do
      Assert.equal (Just O) $
        newGame X 1 1
        >>= flip placeToken 0
        # map currentPlayer

    test "places a token for the current player" do
      Assert.equal (Just [Just X, Just O, Nothing]) $
        newGame X 1 3
        >>= flip placeToken 0
        >>= flip placeToken 0
        >>= flip getColumn 0

  suite "draw" do
    test "renders to string" do
      Assert.equal (Just """|X|_|
|X|O|""") $
        newGame X 2 2
        >>= flip placeToken 0
        >>= flip placeToken 1
        >>= flip placeToken 0
        # map draw
