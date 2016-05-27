import Html exposing (div, button, text)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick)

type Msg = Increment | Decrement

main =
  beginnerProgram
    { model = 0
    , view = view
    , update = update }

view model = div []
  [ button [ onClick Decrement ] [ text "-" ]
  , div [] [model |> toString |> text]
  , button [ onClick Increment ] [ text "+" ]
  ]

update msg model =
  case msg of
    Increment ->
      model + 1
    Decrement ->
      model - 1
