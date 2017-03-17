module EventForm.View exposing (form)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import EventForm.Types exposing (..)


form : Model -> Html Msg
form event =
    Html.form [ onSubmit Submit ]
        [ h1 [] [ text "New event form" ]
        , div [ class "hexagon" ] []
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
            [ type_ <| inputType field
            , value currentValue
            , required True
            , onInput (Input field)
            ]
            []
        ]


inputType : EventField -> String
inputType field =
    case field of
        Name ->
            "text"

        DateStart ->
            "date"

        DateEnd ->
            "date"


labelText : EventField -> String
labelText field =
    case field of
        Name ->
            "Name"

        DateStart ->
            "Start Date"

        DateEnd ->
            "End Date"
