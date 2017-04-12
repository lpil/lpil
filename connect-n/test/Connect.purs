module Test.Connect (tests) where

import Prelude
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck (Result(), (===))
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Ord (abs)
import Connect (Player(..), newGame)

tests :: TestSuite _
tests =
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
