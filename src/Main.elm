module Main exposing (..)

import Html exposing (..)
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
    { month : Date.Month
    , day : Int
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
    { currentDate = Nothing } ! []


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
        { month = Date.month date
        , day = Date.day date
        , hour = Date.hour date
        , minute = Date.minute date
        , second = Date.second date
        }


view : Model -> Html a
view model =
    case Maybe.map to28Hour model.currentDate of
        Nothing ->
            div [] []

        Just date ->
            div []
                [ span [] [ date.month |> toString |> text ]
                , span [] [ date.day |> toString |> text ]
                , span [] [ date.hour |> toString |> text ]
                , span [] [ date.minute |> toString |> text ]
                , span [] [ date.second |> toString |> text ]
                ]
