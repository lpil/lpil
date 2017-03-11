module Types exposing (..)

import Event exposing (Event)
import EventForm.Types as EventForm


type alias Model =
    { newEvent : Event
    , idToken : String
    }


type alias Flags =
    { idToken : String
    }


type Msg
    = LogOut
    | NewEventMsg EventForm.Msg
