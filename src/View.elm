module View exposing (root)

import Html exposing (..)
import Html.Events exposing (onClick)
import Types exposing (..)
import NewEvent.View


root : Model -> Html Msg
root model =
    div []
        [ text "hello, world!"
        , button [ onClick LogOut ] [ text "Log out" ]
        , NewEvent.View.form model.newEvent
        ]
