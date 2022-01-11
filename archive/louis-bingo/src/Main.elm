module Main exposing (main)

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Board exposing (Board)
import Trope


type alias Flags =
    {}


type alias Model =
    { board : Board }


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


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { board = Board.new
      }
    , Cmd.none
    )


update : a -> b -> ( b, Cmd c )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [ class "title" ] [ text "Louis Bingo!" ]
        , boardView model.board
        ]


boardView : Board -> Html Msg
boardView board =
    div [ class "board" ]
        [ cellView board.cell0
        , cellView board.cell1
        , cellView board.cell2
        , cellView board.cell3
        ]


cellView : Board.Cell -> Html Msg
cellView cell =
    a [ class "cell" ] [ cell.trope |> Trope.toString |> text ]
