import Html exposing (..)
import Html.App
import Html.Events
import Random

main =
  Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- Model

type alias Model =
  Int

init : (Model, Cmd Msg)
init =
  (1, Cmd.none)

-- Subscriptions

subscriptions model =
  Sub.none

-- Update

type Msg
  = Roll
  | Result Int

update action model =
  case action of
    Roll ->
      (model, Random.generate Result (Random.int 1 6))

    Result value ->
      (value, Cmd.none)

-- view

view model = div []
  [ h1 [] [ text "Roll the dice!" ]
  , h1 [] [ text <| toString model ]
  , button [ Html.Events.onClick Roll ] [ text "Clicky" ]
  ]
