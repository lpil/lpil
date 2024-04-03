module Lib exposing (..)

import Json.Decode exposing (..)
import String


decoder1 : Decoder String
decoder1 =
    ("name" := string)


decoder2 : Decoder ( Int, String )
decoder2 =
    object2 (,)
        ("age" := int)
        ("role" := string)


decoder3 : Decoder String
decoder3 =
    object3 decoder3Sentence
        ("name" := string)
        ("age" := int)
        ("role" := string)


decoder3Sentence : String -> Int -> String -> String
decoder3Sentence name age role =
    String.concat [ name, " is a ", toString age, " year old ", role ]


decoder4 : Decoder (List Int)
decoder4 =
    list int


decoder5 : Decoder ( String, Maybe String )
decoder5 =
    object2 (,)
        ("name" := string)
        (maybe ("role" := string))


type alias User =
    { email : String
    , token : String
    }


decoder6 : Decoder User
decoder6 =
    at [ "user" ] <|
        object2 User
            ("email" := string)
            ("token" := string)
