port module Main exposing (..)

import Test exposing (..)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit <|
        describe "all tests"
            []


port emit : ( String, Value ) -> Cmd msg
