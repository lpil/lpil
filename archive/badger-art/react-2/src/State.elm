module State exposing (Point, Model, Msg, init, update)

import Array exposing (Array)


type alias Point =
    { x : Int
    , y : Int
    , delay : Int
    }


type alias Model =
    { points : Array Point
    , connections : List ( Point, Point )
    }


init : ( Model, Cmd Msg )
init =
    ( { points = points
      , connections = connections points
      }
    , Cmd.none
    )


points : Array Point
points =
    Array.fromList
        [ { delay = 100, x = 3, y = 44 }
        , { delay = 1820, x = 20, y = 14 }
        , { delay = 930, x = 32, y = 110 }
        , { delay = 490, x = 45, y = -10 }
        , { delay = 1390, x = 62, y = 31 }
        , { delay = 150, x = 75, y = 87 }
        , { delay = 40, x = 73, y = -18 }
        , { delay = 1100, x = 91, y = 86 }
        , { delay = 900, x = 110, y = 150 }
        ]


connections : Array Point -> List ( Point, Point )
connections points =
    List.filterMap (getConnection points)
        [ ( 0, 1 )
        , ( 0, 2 )
        , ( 1, 2 )
        , ( 1, 3 )
        , ( 2, 3 )
        , ( 4, 5 )
        , ( 4, 6 )
        , ( 5, 6 )
        , ( 5, 7 )
        , ( 6, 7 )
        , ( 7, 8 )
        ]


getConnection : Array Point -> ( Int, Int ) -> Maybe ( Point, Point )
getConnection points ( i1, i2 ) =
    case ( Array.get i1 points, Array.get i2 points ) of
        ( Just p1, Just p2 ) ->
            Just ( p1, p2 )

        _ ->
            Nothing



-- Update


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )
