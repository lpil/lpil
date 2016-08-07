module Tests exposing (..)

import Test exposing (..)
import Expect exposing (..)
import Json.Decode exposing (decodeString)
import Lib exposing (..)


all : Test
all =
    describe "A Test Suite"
        [ test "pulling one value out of a JSON object" <|
            \() ->
                """
                { "name": "Tim" }
                """
                    |> decodeString decoder1
                    |> equal
                        (Ok "Tim")
        , test "pulling two values out of a JSON object" <|
            \() ->
                """
                { "name": "Sarah", "age": 32, "role": "engineer" }
                """
                    |> decodeString decoder2
                    |> equal
                        (Ok ( 32, "engineer" ))
        , test "using a different grouping function" <|
            \() ->
                """
                { "name": "Sarah", "age": 32, "role": "engineer" }
                """
                    |> decodeString decoder3
                    |> equal
                        (Ok "Sarah is a 32 year old engineer")
        ]
