module Bob exposing (hey)

import String
import Regex


hey : String -> String
hey msg =
    if (containsAlphabeticals msg) && (isUppercase msg) then
        "Whoa, chill out!"
    else if String.endsWith "?" msg then
        "Sure."
    else if isSilence msg then
        "Fine. Be that way!"
    else
        "Whatever."


isUppercase : String -> Bool
isUppercase string =
    String.toUpper string == string


containsAlphabeticals : String -> Bool
containsAlphabeticals string =
    "[a-zA-Z]"
        |> Regex.regex
        |> runRegex string


isSilence : String -> Bool
isSilence string =
    "^\\s*$"
        |> Regex.regex
        |> runRegex string


runRegex : String -> Regex.Regex -> Bool
runRegex =
    flip Regex.contains
