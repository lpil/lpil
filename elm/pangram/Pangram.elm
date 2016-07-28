module Pangram exposing (..)

import Regex exposing (replace, regex, HowMany(All))
import Set exposing (Set)
import String
import List

isPangram : String -> Bool
isPangram string =
    let
        chars =
            string
            |> String.toLower
            |> alphasOnly
            |> String.toList
            |> Set.fromList
    in
       chars == alphabet


alphasOnly : String -> String
alphasOnly =
    replace All (regex "[^a-z]") (\_ -> "")


alphabet : Set Char
alphabet =
    "abcdefghijklmnopqrstuvwxyz"
    |> String.toList
    |> Set.fromList

