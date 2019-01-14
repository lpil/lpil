module View exposing (root)

import Array exposing (Array)
import Colors exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (style)
import List
import State exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


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
            ((mapPoints model.points) ++ (mapConnections model.connections))
        ]


mapConnections : List ( Point, Point ) -> List (Svg a)
mapConnections connections =
    List.map connectionLine connections


connectionLine : ( Point, Point ) -> Svg a
connectionLine ( p1, p2 ) =
    line
        [ strokeDasharray "7, 7"
        , p1.x |> percent |> x1
        , p1.y |> percent |> y1
        , p2.x |> percent |> x2
        , p2.y |> percent |> y2
        , strokeWidth "3"
        , stroke grey
        ]
        []


mapPoints : Array Point -> List (Svg a)
mapPoints points =
    (Array.toList <| Array.map mapPoint points)


ringSize : number -> number
ringSize index =
    index * 20 + 10


mapPoint : Point -> Svg a
mapPoint point =
    g [] <|
        dot point
            :: List.indexedMap (ring point) [1..4]


ring : Point -> Int -> Int -> Svg a
ring point index radius =
    circle
        [ cx (percent point.x)
        , cy (percent point.y)
        , r (radius |> ringSize |> px)
        , fill "none"
        , strokeWidth "0.5"
        , opacity "0"
        , stroke grey
        , class "header-map__ring"
        , Html.Attributes.style
            [ ( "animation-delay", (delay index point.delay) )
            ]
        ]
        []


delay : Int -> Int -> String
delay index delay =
    (toString (index * 100 + delay)) ++ "ms"


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
