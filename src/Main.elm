module Main exposing (..)

import Html
import Types exposing (..)
import State
import View
import Backend
import Event.CreateEvent
import EventForm.State


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = View.root
        , update = State.update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        requestBuilder =
            Backend.cmd flags.idToken flags.endpoint
    in
        { newEvent = EventForm.State.init
        , createEvent = Event.CreateEvent.cmd requestBuilder
        , events = []
        }
            ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
