module Counter exposing (Model, Msg, init, update, view)

import Html.Events exposing (onClick)
import Html exposing (..)

-- Model

type alias Model =
  Int

init : Model
init =
  0

-- Update

type Msg
  = Increment
  | Decrement
  | Reset

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

    Reset ->
      init


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- View

view model = div []
  [ button [ onClick Decrement ] [ text "-" ]
  , div [] [model |> toString |> text]
  , button [ onClick Increment ] [ text "+" ]
  , br [] []
  , button [ onClick Reset ] [ text "Reset" ]
  , br [] []
  ]
