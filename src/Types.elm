module Types exposing (..)

import Time exposing (Time)


type alias Model =
    { lastTick : Time
    }


type Msg
    = Click
    | Frame Time
