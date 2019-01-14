module View exposing (root)

import Types exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)


root : Model -> Html Msg
root model =
    div []
        [ button [ onClick Click ] [ text "Click me!" ]
        ]
