module NewEvent.View exposing (form)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Types exposing (..)
import NewEvent.Types exposing (..)


form : Event -> Html Msg
form event =
    Html.form [ onSubmit NewEventSubmit ]
        [ h1 [] [ text "New event form" ]
        , eventInput event.name Name
        , eventInput event.dateStart EventStart
        , eventInput event.dateEnd EventEnd
        , div [] [ button [] [ text "Save" ] ]
        ]


eventInput : String -> EventField -> Html Msg
eventInput currentValue field =
    div []
        [ label [] [ text <| labelText field ]
        , input
            [ type_ "text"
            , value currentValue
            , onInput (NewEventInput field)
            ]
            []
        ]


labelText : EventField -> String
labelText field =
    case field of
        Name ->
            "Event Name"

        EventStart ->
            "Start Date"

        EventEnd ->
            "End Date"
