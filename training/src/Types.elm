module Types exposing (..)

import Http
import Date exposing (Date)
import Event exposing (Event)
import EventForm.Types as EventForm


type alias Model =
    { events : Maybe (List Event)
    , newEvent : EventForm.Model
    , createEvent : Event -> Cmd Msg
    , fetchEvents : Cmd Msg
    , currentDate : Maybe Date
    , noNetwork : Bool
    }


type alias Flags =
    { idToken : String
    , endpoint : String
    , userName : String
    , userEmail : String
    }


type Msg
    = LogOut
    | CurrentDate Date
    | NewEventMsg EventForm.Msg
    | FailResponse Http.Error
    | CreateUserResponse ()
    | CreateEventResponse ()
    | FetchEventsResponse (List Event)
