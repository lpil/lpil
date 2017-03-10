module Main exposing (..)

import Html exposing (..)


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


type alias Model =
    {}


type alias Flags =
    {}


type Msg
    = Noop


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( {}, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ text "hello, world!" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
