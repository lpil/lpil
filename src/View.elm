module View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import View.Spinner
import Types exposing (..)
import EventForm.View as EventForm


root : Model -> Html Msg
root model =
    div []
        [ header model
        , eventTiles
        , View.Spinner.root True
        ]


header : Model -> Html Msg
header model =
    let
        navBar =
            nav [ class "header--nav" ]
                [ a [ onClick LogOut, class "header--logout" ]
                    [ text "Log out" ]
                ]
    in
        div [ class "header" ]
            [ navBar
            , map NewEventMsg (EventForm.form model.newEvent)
            ]


eventTiles : Html Msg
eventTiles =
    div [ class "event-tiles" ]
        [ eventTile, eventTile, eventTile, eventTile, eventTile ]


eventTile : Html Msg
eventTile =
    div [ class "event-tile" ]
        [ text "A super event." ]
