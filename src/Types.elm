module Types exposing (..)

import Array exposing (Array)
import Event exposing (Event)
import EventForm.Types as EventForm


type alias Model =
    { newEvent : Event
    , events : Array Event
    }


type alias Flags =
    {}


type Msg
    = LogOut
    | EventFormMsg EventForm.Msg
