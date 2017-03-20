module Main exposing (..)

import Html exposing (..)
import Time exposing (Time, second)


type Msg
    = Tick Time


type alias Model =
    {}


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
    {} ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []


view : Model -> Html a
view model =
    div [] [ text "clock" ]
