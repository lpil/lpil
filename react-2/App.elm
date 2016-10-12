module App exposing (main)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.App
import List


main =
    Html.App.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- Model


type alias Point =
    { x : Int
    , y : Int
    }


type alias Model =
    { points : List Point
    }


init : ( Model, Cmd Msg )
init =
    ( { points =
            [ { x = 100, y = 100 }
            ]
      }
    , Cmd.none
    )



-- Update


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub a
subscriptions model =
    Sub.none



-- View


view : Model -> Svg Msg
view model =
    svg [ version "1.1", x "0", y "0", viewBox "0 0 323.141 322.95" ]
        (List.map mapPoint model.points)


grey : String
grey =
    "rgb(74, 74, 74)"


ringSize : number -> number
ringSize index =
    index * 10 + 3


mapPoint : Point -> Svg a
mapPoint point =
    g [] <|
        dot point
            :: List.map
                (ringSize >> (ring point))
                [ 1, 2, 3, 4 ]


ring : Point -> number -> Svg a
ring point radius =
    circle
        [ cx (px point.x)
        , cy (px point.y)
        , r (px radius)
        , fill "none"
        , strokeWidth "0.5"
        , opacity "0.3"
        , stroke grey
        ]
        []


dot : Point -> Svg a
dot point =
    circle
        [ cx (px point.x)
        , cy (px point.y)
        , r (px 3)
        , fill grey
        ]
        []


px : number -> String
px num =
    (toString num) ++ "px"
