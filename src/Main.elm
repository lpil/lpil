module Main exposing (main)

import Html.App as App
import Html exposing (..)


type alias Flags =
    {}


type alias Model =
    {}


type Msg
    = Noop


main : Program Flags
main =
    App.programWithFlags
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


subscriptions : Model -> Sub a
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [] [ text "Hello!" ]


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( {}, Cmd.none )


update : a -> b -> ( b, Cmd c )
update msg model =
    ( model, Cmd.none )
