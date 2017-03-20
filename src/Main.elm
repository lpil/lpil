module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Task
import Maybe
import Time exposing (Time, second)
import Date exposing (Date)


type Msg
    = Tick Time
    | CurrentDate Date


type alias Model =
    { currentDate : Maybe Date }


type alias Date28Hour =
    { year : Int
    , month : Date.Month
    , weekday : Int
    , monthday : Int
    , second : Int
    , minute : Int
    , hour : Int
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    { currentDate = Nothing }
        ! [ Task.perform CurrentDate Date.now ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            model ! [ Task.perform CurrentDate Date.now ]

        CurrentDate date ->
            { model | currentDate = Just date } ! []


to28Hour : Date -> Date28Hour
to28Hour date =
    let
        day =
            Date.dayOfWeek date

        dayNum =
            case day of
                Date.Mon ->
                    0

                Date.Tue ->
                    1

                Date.Wed ->
                    2

                Date.Thu ->
                    3

                Date.Fri ->
                    4

                Date.Sat ->
                    5

                Date.Sun ->
                    6

        dayHours =
            dayNum * 24

        day28Hour =
            rem dayHours 28
    in
        { year = Date.year date
        , month = Date.month date
        , weekday = day28Hour
        , monthday = Date.day date
        , hour = Date.hour date
        , minute = Date.minute date
        , second = Date.second date
        }


monthName : Date.Month -> String
monthName month =
    case month of
        Date.Jan ->
            "January"

        Date.Feb ->
            "February"

        Date.Mar ->
            "March"

        Date.Apr ->
            "April"

        Date.May ->
            "May"

        Date.Jun ->
            "June"

        Date.Jul ->
            "July"

        Date.Aug ->
            "August"

        Date.Sep ->
            "September"

        Date.Oct ->
            "October"

        Date.Nov ->
            "November"

        Date.Dec ->
            "December"


padNumber : Int -> String
padNumber n =
    if n < 10 then
        "0" ++ (toString n)
    else
        toString n


weekdayName : Int -> String
weekdayName weekday =
    case weekday of
        0 ->
            "Monday"

        1 ->
            "Tuesday"

        2 ->
            "Wednesday"

        3 ->
            "Thursday"

        4 ->
            "Friday"

        _ ->
            "Saturday"


view : Model -> Html Msg
view model =
    case Maybe.map to28Hour model.currentDate of
        Nothing ->
            div [] []

        Just date ->
            clockView date


clockView : Date28Hour -> Html Msg
clockView date =
    let
        dateInfo =
            div [ class "clock-date" ]
                [ span [ class "weekday" ] [ date.weekday |> weekdayName |> text ]
                , text ", "
                , span [ class "monthday" ] [ date.monthday |> toString |> text ]
                , text " "
                , span [ class "month" ] [ date.month |> monthName |> text ]
                , text " "
                , span [ class "year" ] [ date.year |> toString |> text ]
                ]

        timeInfo =
            div [ class "clock-time" ]
                [ span [ class "hour" ] [ date.hour |> padNumber |> text ]
                , text ":"
                , span [ class "minute" ] [ date.minute |> padNumber |> text ]
                , text ":"
                , span [ class "second" ] [ date.second |> padNumber |> text ]
                ]
    in
        div [ class "clock-container" ]
            [ div [ class "clock" ] [ dateInfo, timeInfo ]
            ]
