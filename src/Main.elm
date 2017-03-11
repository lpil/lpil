module Main exposing (..)

import Html
import Types exposing (..)
import State
import View


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = View.root
        , init = State.init
        , update = State.update
        , subscriptions = State.subscriptions
        }
