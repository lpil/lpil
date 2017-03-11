module EventForm.View exposing (form)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import EventForm.Types exposing (..)
import Event exposing (Event)


form : Model -> Html Msg
form event =
    Html.form [ onSubmit Submit ]
        [ h1 [] [ text "New event form" ]
        , eventInput event.name Name
        , eventInput event.dateStart DateStart
        , eventInput event.dateEnd DateEnd
        , div [] [ button [] [ text "Submit" ] ]
        ]


eventInput : String -> EventField -> Html Msg
eventInput currentValue field =
    div []
        [ label [] [ text <| labelText field ]
        , input
            [ type_ "text"
            , value currentValue
            , onInput (Input field)
            ]
            []
        ]


labelText : EventField -> String
labelText field =
    case field of
        Name ->
            "Event Name"

        DateStart ->
            "Start Date"

        DateEnd ->
            "End Date"
