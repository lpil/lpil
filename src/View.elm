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
    let
        tiles =
            div [ class "tiles" ] <|
                newEventTile model
                    :: (List.map eventTile model.events)
    in
        div []
            [ View.Spinner.root True
            , a [ onClick LogOut, class "logout" ] [ text "Log out" ]
            , tiles
            ]


newEventTile : Model -> Html Msg
newEventTile model =
    div [ class "new-event-tile" ]
        [ map NewEventMsg (EventForm.form model.newEvent) ]


eventTile : Event -> Html Msg
eventTile event =
    let
        detail name value =
            div [ class "event-tile__field" ]
                [ label [ class "event-tile__label" ] [ text name ]
                , div [ class "event-tile__value" ] [ text value ]
                ]
    in
        div [ class "event-tile" ]
            [ h3 [] [ text event.name ]
            , detail "Start Date" event.dateStart
            , detail "End Date" event.dateEnd
            ]
