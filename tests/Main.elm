port module Main exposing (..)

import Test exposing (..)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Event.CreateEventTests


main : TestProgram
main =
    run emit <|
        describe "all tests"
            [ Event.CreateEventTests.all
            ]


port emit : ( String, Value ) -> Cmd msg
