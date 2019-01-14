module EventForm.Types exposing (..)

import Array exposing (Array)


type alias Model =
    { name : String
    , url : String
    , dateStart : String
    , dateEnd : String
    , interestedPeople : Array String
    }


type Msg
    = Input EventField String
    | Submit


type EventField
    = Name
    | Url
    | DateStart
    | DateEnd
    | InterestedPeople Int
