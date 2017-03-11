module Main exposing (..)

import Html
import Array
import Types exposing (..)
import View
import Update
import EventForm.Update


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = View.root
        , update = Update.update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    { newEvent = EventForm.Update.newEvent
    , events = Array.fromList []
    }
        ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
