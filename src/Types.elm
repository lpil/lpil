module Types exposing (..)

import Array exposing (Array)
import NewEvent.Types as NewEvent


type alias Event =
    { name : String
    , dateStart : String
    , dateEnd : String
    }


type alias Model =
    { newEvent : Event
    , events : Array Event
    }


type alias Flags =
    {}


type Msg
    = NewEventInput NewEvent.EventField String
    | NewEventSubmit
