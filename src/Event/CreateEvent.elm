module Event.CreateEvent exposing (cmd, query, rawQuery)

import Backend exposing (RequestBuilder)
import Json.Encode as Encode
import Json.Decode as Decode
import Types exposing (Msg(..))
import EventForm.Types exposing (Model)


{-| Construct a Cmd that attempts create a user.

It does this by sending a GraphQL mutation to the backend via HTTP.
-}
cmd : RequestBuilder Int -> Model -> Cmd Msg
cmd builder model =
    builder Decode.int CreateUserResponse (query model)


query : Model -> Encode.Value
query { name, dateStart, dateEnd } =
    Encode.object
        [ ( "operationName", Encode.string "CreateEvent" )
        , ( "query", Encode.string rawQuery )
        , ( "variables"
          , Encode.object
                [ ( "name", Encode.string name )
                , ( "dateStart", Encode.string dateStart )
                , ( "dateEnd", Encode.string dateEnd )
                ]
          )
        ]


rawQuery : String
rawQuery =
    """
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
