module Backend.FetchEvents exposing (query)

import Backend
import Json.Decode as Decode
import Types exposing (Msg(..))
import Event exposing (Event)


{-| Attempts to create an event.

-}
query : Backend.Query (List Event)
query =
    { name = "FetchEvents"
    , responseMsg = FetchEventsResponse
    , responseDecoder = decoder
    , variables = []
    , graphQl = """
query FetchEvents {
    allEvents(orderBy: dateStart_ASC) {
        id
        name
        url
        dateStart
        dateEnd
    }
}
"""
    }


decoder : Decode.Decoder (List Event)
decoder =
    let
        eventDecoder =
            Decode.map4 Event
                (Decode.field "name" Decode.string)
                (Decode.field "url" Decode.string)
                (Decode.field "dateStart" Decode.string)
                (Decode.field "dateEnd" Decode.string)
    in
        Decode.at [ "data", "allEvents" ] (Decode.list eventDecoder)
