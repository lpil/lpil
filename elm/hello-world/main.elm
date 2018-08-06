-- In order for this to compile I needed to use the package manager to install
-- the elm-lang/html package.
--  $ elm package install elm-lang/html

import Html exposing (text, div, li, ul)
import Html.Attributes exposing (class)

main =
  div []
    [text "Hello, World!"
    , ul [class "language-list"]
      [ li [] [text "Clojure"]
      , li [] [text "Elixir"]
      , li [] [text "Elm"]
      , li [] [text "Erlang"]
      , li [] [text "Haskell"]
      , li [] [text "Javascript"]
      , li [] [text "Ruby"]
      ]
    ]
