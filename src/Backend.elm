module Backend exposing (Query, cmd)

import Http exposing (Request)
import Json.Encode as Encode
import Json.Decode exposing (Decoder)
import Types exposing (..)


type alias Endpoint =
    String


type alias IdToken =
    String


type alias Query responseData =
    { name : String
    , graphQl : String
    , variables : List ( String, Encode.Value )
    , responseMsg : responseData -> Msg
    , responseDecoder : Decoder responseData
    }


{-| Construct a Cmd that sends a GraphQL query to the backend.

-}
cmd : IdToken -> Endpoint -> Query responseData -> Cmd Msg
cmd idToken endpoint query =
    let
        headers =
            [ Http.header "Authorization" ("Bearer " ++ idToken)
            ]

        requestPayload =
            Http.request
                { method = "POST"
                , headers = headers
                , url = endpoint
                , body = query |> requestBody |> Http.jsonBody
                , expect = Http.expectJson query.responseDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send (dispatchMsg query.responseMsg) requestPayload


requestBody : Query a -> Encode.Value
requestBody query =
    Encode.object
        [ ( "operationName", Encode.string query.name )
        , ( "query", Encode.string query.graphQl )
        , ( "variables", Encode.object query.variables )
        ]


dispatchMsg : (resp -> Msg) -> Result Http.Error resp -> Msg
dispatchMsg msg result =
    case result of
        Ok resp ->
            msg resp

        Err error ->
            FailResponse error
