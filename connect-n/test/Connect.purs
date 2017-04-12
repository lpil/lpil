module Test.Connect (tests) where

import Prelude
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck (Result(), (===))
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Ord (abs)
import Connect (Player(..), newGame)

tests :: TestSuite _
tests =
  suite "Connect.newGame" do
    test "Just Game with > 0 board size ints" do
      quickCheck (\x y -> isJust (newGame X (pos x) (pos y)) === true)

    test "Nothing with > 0 numCols" do
      quickCheck (\x y -> isNothing (newGame X (neg x) (pos y)) === true)

    test "Nothing with > 0 maxSize" do
      quickCheck (\x y -> isNothing (newGame X (pos x) (neg y)) === true)

  where
    pos x =
      abs x + 1

    neg x =
      0 - (abs x)
