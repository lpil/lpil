module State exposing (..)

import Array exposing (Array)


-- Data


type alias Point =
    { x : Int
    , y : Int
    }


type alias Model =
    { points : Array Point
    }


init : ( Model, Cmd Msg )
init =
    ( { points = points
      }
    , Cmd.none
    )


points : Array Point
points =
    Array.fromList
        [ { x = 3, y = 44 }
        , { x = 20, y = 14 }
        , { x = 32, y = 110 }
        , { x = 45, y = -10 }
        , { x = 62, y = 31 }
        , { x = 75, y = 87 }
        , { x = 91, y = 86 }
        ]



-- Update


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )
