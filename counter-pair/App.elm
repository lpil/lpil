import Counter
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.App

main : Program Never
main =
  Html.App.program
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }

-- Model

type alias Model =
  { topCounter : Counter.Model
  , botCounter : Counter.Model
  }

init : (Model, Cmd a)
init =
  ( { topCounter = Counter.init
    , botCounter = Counter.init
    }
  , Cmd.none
  )

-- Update

type Msg
  = Top Counter.Msg
  | Bot Counter.Msg
  | Swap

update : Msg -> Model -> (Model, Cmd a)
update message model =
  case message of
    Top msg ->
      ( { model | topCounter = Counter.update msg model.topCounter }
      , Cmd.none
      )

    Bot msg ->
      ( { model | botCounter = Counter.update msg model.botCounter }
      , Cmd.none
      )

    Swap ->
      (swapCounters(model), Cmd.none)

swapCounters : Model -> Model
swapCounters model =
  { model
  | botCounter = model.topCounter
  , topCounter = model.botCounter
  }


-- Subscriptions

subscriptions : Model -> Sub a
subscriptions model =
  Sub.none


-- View

view : Model -> Html Msg
view model =
  div []
    [ Html.App.map Top (Counter.view model.topCounter)
    , br [] []
    , Html.App.map Bot (Counter.view model.botCounter)
    , br [] []
    , button [ onClick Swap ] [ text "Swap" ]
    ]
