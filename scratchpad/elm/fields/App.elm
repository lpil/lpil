import Html exposing (div, input, text)
import Html.Events exposing (onInput)
import Html.Attributes exposing (placeholder, style)
import Html.App
import String

type Msg = NewContent String

main =
  Html.App.beginnerProgram
    { model = ""
    , view = view
    , update = update }

update (NewContent content) _ =
  content

view model = div []
  [ input [ placeholder "Type on me!", onInput NewContent, myStyle ] []
  , div [ myStyle ] [ text (String.reverse model) ]
  ]

myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
