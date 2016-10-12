module App exposing (main)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (style)
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
            [ { x = 3, y = 44 }
            , { x = 20, y = 14 }
            , { x = 32, y = 110 }
            , { x = 45, y = -10 }
            , { x = 62, y = 31 }
            , { x = 75, y = 87 }
            , { x = 91, y = 86 }
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


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style [ ( "background-color", green ) ]
        ]
        [ svg
            [ version "1.1"
            , x "0"
            , y "0"
            , width "100%"
            , height "260px"
            ]
            (List.map mapPoint model.points)
        ]


green : String
green =
    "#cde5b3"


grey : String
grey =
    "rgb(74, 74, 74)"


ringSize : number -> number
ringSize index =
    index * 20 + 10


mapPoint : Point -> Svg a
mapPoint point =
    g [] <|
        dot point
            :: List.map
                (ringSize >> (ring point))
                [1..4]


ring : Point -> number -> Svg a
ring point radius =
    circle
        [ cx (percent point.x)
        , cy (percent point.y)
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
        [ cx (percent point.x)
        , cy (percent point.y)
        , r (px 8)
        , fill grey
        ]
        []


percent : number -> String
percent num =
    (toString num) ++ "%"


px : number -> String
px num =
    (toString num) ++ "px"
