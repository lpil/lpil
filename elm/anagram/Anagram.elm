module Anagram exposing (..)

import Set exposing (Set)


detect : String -> List String -> List String
detect word candidates =
    let
        wordChars =
            toChars word

        isAnagram candidate =
            (String.toLower candidate /= String.toLower word)
                && (wordChars == (toChars candidate))
    in
        candidates
            |> List.filter isAnagram


toChars : String -> List Char
toChars word =
    word
        |> String.toLower
        |> String.toList
        |> List.sort
