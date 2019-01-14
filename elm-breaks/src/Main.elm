module Main exposing (..)

import Html exposing (Html)
import AnimationFrame
import Time exposing (Time)
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
    { lastTick = 0
    }
        ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.times Frame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            model ! [ Sample.play "Hello, JS!" ]

        Frame time ->
            frame time model ! []


frame : Time -> Model -> Model
frame time model =
    if time > model.lastTick + 1000 then
        let
            _ =
                Debug.log "1 second has passed" time
        in
            { model | lastTick = time }
    else
        model
