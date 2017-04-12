module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Test.Unit.Main (runTest)
import Test.Unit (suite)
import Test.Connect as Connect

main :: Eff _ Unit
main = runTest do
  suite "tests" do
    Connect.tests
