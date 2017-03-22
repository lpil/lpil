module View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import View.Spinner
import Types exposing (..)
import EventForm.View as EventForm
import Event exposing (Event)


root : Model -> Html Msg
root model =
    div []
        [ View.Spinner.root True
        , header model
        , eventTiles model.events
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


eventTiles : List Event -> Html Msg
eventTiles events =
    div [ class "event-tiles" ] (List.map eventTile events)


eventTile : Event -> Html Msg
eventTile event =
    let
        detail name value =
            div []
                [ label [ class "event-tile__label" ] [ text name ]
                , span [] [ text value ]
                ]
    in
        div [ class "event-tile" ]
            [ h3 [ class "event-tile__name" ] [ text event.name ]
            , detail "Start Date" event.dateStart
            , detail "End Date" event.dateEnd
            ]
