import Time exposing (Time, second)
import Html exposing (Html)
import Html.App
import Svg exposing (..)
import Svg.Attributes exposing (..)

main =
  Html.App.program
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }

-- Model

type alias Model =
  Time

init : (Model, Cmd Msg)
init =
  (0, Cmd.none)

-- Update

type Msg
  = Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick time ->
      (time, Cmd.none)


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick

-- View



hand : Float -> Float -> Html a
hand angle length =
  let
    x = toString (50 + length * cos angle)
    y = toString (50 + length * sin angle)
  in
    line [ x1 "50", y1 "50", x2 x, y2 y, stroke "#023963" ] []

view : Model -> Html Msg
view model =
  let
    secondAngle =
      turns (Time.inMinutes model)
    minuteAngle =
      turns (Time.inHours model)
    hourAngle =
      turns (Time.inHours model / 12)
  in
    svg [ viewBox "0 0 100 100", width "300px" ]
      [ circle [ cx "50", cy "50", r "45", fill "#ccc" ] []
      , hand secondAngle 35
      , hand minuteAngle 40
      , hand hourAngle 25
      ]

