module Test.Main where

import Prelude (Unit, ($), discard)
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Either (Either(..))
import Data.Stylesheet (Ast(..), parse)

main :: forall e. Eff
  ( console :: CONSOLE
  , testOutput :: TESTOUTPUT
  , avar :: AVAR
  | e
  ) Unit
main = runTest do
  suite "Data.Stylesheet" do
    suite "parse" do
      test "blank source" do
        let source = ""
        Assert.equal (parse source) $ Left []

      {-- test "empty function" do --}
      {--   let source = """ --}
      {--   @function main { --}
      {--     return: "Hello, world!"; --}
      {--   } --}
      {--   """ --}
      {--   let nodes = [ Function { name: "main" } ] --}
      {--   Assert.equal (parse source) (Left nodes) --}
