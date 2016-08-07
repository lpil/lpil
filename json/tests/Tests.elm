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
        , test "decoding lists" <|
            \() ->
                """
                [ 1, 2, 3, 4, 5, 6, 7 ]
                """
                    |> decodeString decoder4
                    |> equal
                        (Ok [ 1, 2, 3, 4, 5, 6, 7 ])
        , test "optional value present" <|
            \() ->
                """
                { "name": "Sarah", "role": "boss" }
                """
                    |> decodeString decoder5
                    |> equal
                        (Ok ( "Sarah", Just "boss" ))
        , test "optional value not present" <|
            \() ->
                """
                { "name": "Sarah" }
                """
                    |> decodeString decoder5
                    |> equal
                        (Ok ( "Sarah", Nothing ))
        , test "optional value not present" <|
            \() ->
                """
                {
                    "user": {
                        "email": "some@thi.ng",
                        "token": "this-is-a-token"
                    }
                }
                """
                    |> decodeString decoder6
                    |> equal
                        (Ok
                            { email = "some@thi.ng"
                            , token = "this-is-a-token"
                            }
                        )
        ]
