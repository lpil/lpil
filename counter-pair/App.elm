import Counter
import Html exposing (..)
import Html.App

main : Program Never
main =
  Html.App.program
    { init = init 0 0
    , subscriptions = subscriptions
    , update = update
    , view = view
    }

-- Model

type alias Model =
  { topCounter : Counter.Model
  , botCounter : Counter.Model
  }

init : Int -> Int -> (Model, Cmd a)
init top bot =
  ( { topCounter = Counter.init top
    , botCounter = Counter.init bot }
  , Cmd.none
  )

-- Update

type Msg
  = Top Counter.Msg
  | Bot Counter.Msg

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
    -- , button [ onClick Reset ] [ text "RESET" ]
    ]
