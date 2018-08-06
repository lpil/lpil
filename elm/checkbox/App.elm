import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck)
import Html.App
import Debug

main =
  Html.App.beginnerProgram
    { model = defaultModel
    , view = view
    , update = update
    }


-- Model

type alias Model =
  { red : Bool
  , italic : Bool
  , underline : Bool
  }

defaultModel =
  Model False True False


-- Update

type Msg
  = Red Bool
  | Underline Bool
  | Italic Bool

update action model =
  case action of
    Red bool ->
      { model | red = bool }

    Italic bool ->
      { model | italic = bool }

    Underline bool ->
      { model | underline = bool }

-- view

view model = div []
  [ h1 [ h1Style model ] [ text "Hello, world!"]
  , label []
      [ br [] []
      , input [type' "checkbox", checked model.red, onCheck Red] [ ]
      , text "Red"
      ]
  , label []
      [ br [] []
      , input [type' "checkbox", checked model.italic, onCheck Italic] []
      , text "Italic"
      ]
  , label []
      [ br [] []
      , input [type' "checkbox", checked model.underline, onCheck Underline] []
      , text "Underline"
      ]
  ]

h1Style model =
  []
  |> prependIf model.red ("color", "red")
  |> prependIf model.italic ("font-style", "Italic")
  |> prependIf model.underline ("text-decoration", "underline")
  |> style

prependIf pred style acc =
  if pred then
    style :: acc
  else
    acc
