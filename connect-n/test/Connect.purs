module Test.Connect (tests) where

import Prelude
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Data.Maybe (Maybe(..), isJust, isNothing)
import Connect (Player(..), newGame, columnSize, columnTokens)

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

  suite "Connect.columnTokens" do
    test "columns start with no tokens"
      let
        game =
          newGame X 2 5
      in do
        Assert.equal (Just 0) (game >>= flip columnTokens 0)

        Assert.equal (Just 0) (game >>= flip columnTokens 1)

    test "Nothing for out of bounds columns" do
      Assert.equal Nothing
        (newGame O 2 5 >>= flip columnTokens 2)
