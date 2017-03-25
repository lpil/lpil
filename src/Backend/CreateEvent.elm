module Backend.CreateEvent exposing (query)

import Backend
import Json.Encode as Encode
import Json.Decode as Decode
import Types exposing (Msg(..))
import EventForm.Types exposing (Model)


-- TODO: Handle failure


{-| Attempts to create an event.

-}
query : Model -> Backend.Query ()
query { name, dateStart, dateEnd } =
    { name = "CreateEvent"
    , responseMsg = CreateEventResponse
    , responseDecoder = Decode.succeed ()
    , variables =
        [ ( "name", Encode.string name )
        , ( "dateStart", Encode.string dateStart )
        , ( "dateEnd", Encode.string dateEnd )
        ]
    , graphQl = """
mutation CreateEvent(
    $name: String!
    $dateStart: DateTime!
    $dateEnd: DateTime!
) {
    createEvent(
        name: $name
        dateStart: $dateStart
        dateEnd: $dateEnd
    ) {
        id
        name
        dateStart
        dateEnd
    }
}
"""
    }
