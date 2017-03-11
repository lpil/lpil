module EventForm.Types exposing (..)

import Event exposing (Event)


type alias Model =
    Event


type Msg
    = Input EventField String
    | Submit


type EventField
    = Name
    | DateStart
    | DateEnd
