module Main exposing (..)

import Html exposing (Html)
import Types exposing (..)
import View
import Sample


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = View.root
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    {} ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            model ! [ Sample.play "Hello, JS!" ]
