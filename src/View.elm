module View exposing (root)

import Html exposing (..)
import Types exposing (..)
import NewEvent.View


root : Model -> Html Msg
root model =
    div []
        [ text "hello, world!"
        , NewEvent.View.form model.newEvent
        ]
