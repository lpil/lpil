module View exposing (root)

import Html exposing (..)
import Html.Events exposing (onClick)
import Types exposing (..)
import EventForm.View as EventForm


root : Model -> Html Msg
root model =
    div []
        [ text "hello, world!"
        , button [ onClick LogOut ] [ text "Log out" ]
        , map EventFormMsg (EventForm.form model.newEvent)
        ]
