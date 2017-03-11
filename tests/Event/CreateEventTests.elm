module Event.CreateEventTests exposing (..)

import Test exposing (..)
import Expect
import Json.Encode exposing (..)
import Event.CreateEvent as CreateEvent


all : Test
all =
    test "Event.CreateEvent.query" <|
        \() ->
            let
                query =
                    CreateEvent.query
                        { name = "Good event"
                        , dateStart = "2017-12-25"
                        , dateEnd = "2017-12-26"
                        }

                expected =
                    (object
                        [ ( "operationName", string "CreateEvent" )
                        , ( "query", string CreateEvent.rawQuery )
                        , ( "variables"
                          , object
                                [ ( "name", string "Good event" )
                                , ( "dateStart", string "2017-12-25" )
                                , ( "dateEnd", string "2017-12-26" )
                                ]
                          )
                        ]
                    )
            in
                query |> Expect.equal expected
