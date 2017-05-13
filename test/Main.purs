module Test.Main where

import Prelude
import Test.Unit (test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Maybe (Maybe(..))
import Data.Context (textContext)
import Hal as Hal


main :: forall e. Eff ( console :: CONSOLE
                      , testOutput :: TESTOUTPUT
                      , avar :: AVAR
                      | e
                      ) Unit
main = runTest do
  test "quotes" do
    Hal.handle (textContext "") >>=
      Assert.equal Nothing

    Hal.handle (textContext "Hello") >>=
      Assert.equal Nothing

    Hal.handle (textContext "Do YOU read me?") >>=
      Assert.equal (Just "Affirmative, Dave. I read you.")

    Hal.handle (textContext "do you read me?") >>=
      Assert.equal (Just "Affirmative, Dave. I read you.")

    Hal.handle (textContext "Hey Hal, do you read me?") >>=
      Assert.equal (Just "Affirmative, Dave. I read you.")

    Hal.handle (textContext "Open the pod bay doors.") >>=
      Assert.equal (Just "I'm sorry, Dave. I'm afraid I can't do that.")
