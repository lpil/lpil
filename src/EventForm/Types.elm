module EventForm.Types exposing (..)


type alias Model =
    { name : String
    , url : String
    , dateStart : String
    , dateEnd : String
    }


type Msg
    = Input EventField String
    | Submit


type EventField
    = Name
    | Url
    | DateStart
    | DateEnd
