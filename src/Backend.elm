module Backend exposing (cmd, RequestBuilder)

import Http exposing (Request)
import Json.Encode exposing (Value)
import Json.Decode exposing (Decoder)
import Types exposing (..)


type alias Endpoint =
    String


type alias IdToken =
    String


type alias RequestBuilder data =
    Decoder data
    -> (data -> Msg)
    -> Value
    -> Cmd Msg


{-| Construct a Cmd that sends a GraphQL query to the backend.

Partially apply everything except the body for convenience.

-}
cmd : IdToken -> Endpoint -> RequestBuilder responseBody
cmd idToken endpoint respBodyDecoder msg body =
    let
        headers =
            [ Http.header "Authorization" ("Bearer " ++ idToken)
            ]

        requestPayload =
            Http.request
                { method = "POST"
                , headers = headers
                , url = endpoint
                , body = Http.jsonBody body
                , expect = Http.expectJson respBodyDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send (dispatchMsg msg) requestPayload


dispatchMsg : (resp -> Msg) -> Result Http.Error resp -> Msg
dispatchMsg msg result =
    case result of
        Ok resp ->
            msg resp

        Err error ->
            FailResponse error
