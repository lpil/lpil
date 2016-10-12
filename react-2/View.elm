module View exposing (root)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Array
import State exposing (..)
import Colors exposing (..)


root : Model -> Html Msg
root model =
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
            (Array.toList <| Array.map mapPoint model.points)
        ]


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
