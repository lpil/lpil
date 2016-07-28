module Raindrops exposing (raindrops)

import List exposing (foldr)

-- foldr (+) 0 [1,2,3] == 6

raindrops : Int -> String
raindrops num =
    let
        fun =
            \(div, noise) acc ->
                if num % div == 0 then noise ++ acc else acc
        result =
            foldr fun "" rules
    in
        if result == "" then
            toString num
        else
            result


rules : List (number, String)
rules =
    [ (3, "Pling")
    , (5, "Plang")
    , (7, "Plong")
    ]
