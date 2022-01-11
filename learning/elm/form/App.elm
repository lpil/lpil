import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html.App

main =
  Html.App.beginnerProgram
    { model = defaultModel
    , view = view
    , update = update
    }


-- Model

type alias Model =
  { name : String
  , password : String
  , confirmation : String
  }

defaultModel =
  Model "" "" ""


-- Update

type Msg
  = Name String
  | Password String
  | Confirmation String

update action model =
  case action of
    Name value ->
      { model | name = value }

    Password value ->
      { model | password = value }

    Confirmation value ->
      { model | confirmation = value }


-- view

view model = div []
  [ input
      [ placeholder "Name", onInput Name ]
      []

  , input
      [ type' "password", placeholder "Password", onInput Password ]
      []

  , input [
      type' "password", placeholder "Confirmation", onInput Confirmation ]
      []

  , validationView model
  ]

validationView model =
  let
    (color, message) =
      if model.password /= ""
         && model.name /= ""
         && model.password == model.confirmation
      then
        ("green", "Password OK")
      else
        ("red", "Password NOT OK")
  in
     div [ style [("color", color)] ] [ text message ]


