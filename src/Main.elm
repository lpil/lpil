module Main exposing (..)

import Html
import Date
import Task
import Types exposing (..)
import State
import View
import Backend
import Backend.CreateUser
import Backend.CreateEvent
import Backend.FetchEvents
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
        send =
            Backend.cmd flags.idToken flags.endpoint

        createUser =
            send (Backend.CreateUser.query flags.idToken)
    in
        { newEvent = EventForm.State.init
        , createEvent = Backend.CreateEvent.query >> send
        , fetchEvents = send Backend.FetchEvents.query
        , events = Nothing
        , currentDate = Nothing
        }
            ! [ Task.perform CurrentDate Date.now
              , createUser
              ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
