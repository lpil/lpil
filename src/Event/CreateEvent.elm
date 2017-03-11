module Event.CreateEvent exposing (cmd, query, rawQuery)

import Json.Encode as Encode
import Types exposing (Msg(..))
import EventForm.Types exposing (Model)


type alias IdToken =
    String


{-| Construct a Cmd that attempts create a user.

It does this by sending a GraphQL mutation to the backend via HTTP.
-}
cmd : IdToken -> Model -> Cmd Msg
cmd token model =
    Cmd.none


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



{- The entire query needs to look like so:

       {
           "operationName": NAME_OF_QUERY_HERE
           "query": QUERY_STRING_GOES_HERE,
           "variables":"{\n  \"foo\": \"wow\"\n}",
       }

   Note the variables are encoded as JSON. Weird.

-}
