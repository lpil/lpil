module Types exposing (..)

import Http
import Event exposing (Event)
import EventForm.Types as EventForm


type alias Model =
    { newEvent : Event
    , createEvent : Event -> Cmd Msg
    }


type alias Flags =
    { idToken : String
    , endpoint : String
    , userName : String
    , userEmail : String
    }


type Msg
    = LogOut
    | NewEventMsg EventForm.Msg
    | FailResponse Http.Error
    | CreateUserResponse Int
