module View exposing (root)

import Html exposing (..)
import Html.Keyed
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)
import View.Spinner
import Types exposing (..)
import EventForm.View as EventForm
import Event exposing (Event)
import Date exposing (Date)


root : Model -> Html Msg
root model =
    let
        tiles =
            Html.Keyed.node "div" [ class "tiles" ] <|
                newEventTile model
                    :: eventTiles model.events
    in
        div []
            [ View.Spinner.root True
            , a [ onClick LogOut, class "logout" ] [ text "Log out" ]
            , tiles
            ]


newEventTile : Model -> ( String, Html Msg )
newEventTile model =
    let
        form =
            EventForm.form model.newEvent model.currentDate

        elem =
            div [ class "new-event-tile" ]
                [ map NewEventMsg form ]
    in
        ( "newEventTile", elem )


eventTile : Event -> ( String, Html Msg )
eventTile event =
    let
        detail name value =
            div [ class "event-tile__field" ]
                [ label [ class "event-tile__label" ] [ text name ]
                , div [ class "event-tile__value" ] [ text value ]
                ]

        elem =
            div [ class "event-tile" ]
                [ h3 [ class "event-tile__title" ]
                    [ a [ href event.url, target "_blank" ]
                        [ text event.name ]
                    ]
                , detail "Start Date" (formatDate event.dateStart)
                , detail "End Date" (formatDate event.dateEnd)
                ]
    in
        ( event.name, elem )


eventTiles : Maybe (List Event) -> List ( String, Html Msg )
eventTiles maybeEvents =
    case maybeEvents of
        Nothing ->
            [ ( "loading"
              , div [ class "loading-tile" ] [ text "loading!" ]
              )
            ]

        Just events ->
            List.map eventTile events


formatDate : String -> String
formatDate dateString =
    dateString
        |> Date.fromString
        |> Result.map
            (\date ->
                [ toString (Date.day date)
                , toString (Date.month date)
                , toString (Date.year date)
                ]
                    |> String.join " "
            )
        |> Result.withDefault ""
