module EventForm.Types exposing (..)


type alias Model =
    { name : String
    , dateStart : String
    , dateEnd : String
    }


type Msg
    = Input EventField String
    | Submit


type EventField
    = Name
    | DateStart
    | DateEnd
