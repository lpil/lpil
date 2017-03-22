module Types exposing (..)

import Http
import Event exposing (Event)
import EventForm.Types as EventForm


type alias Model =
    { events : List Event
    , newEvent : EventForm.Model
    , createEvent : Event -> Cmd Msg
    , fetchEvents : Cmd Msg
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
    | CreateEventResponse Int
    | CreateUserResponse ()
    | FetchEventsResponse (List Event)
