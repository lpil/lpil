module EventForm.Date exposing (dateYyMmDd)

import Date exposing (Date, Month)


dateYyMmDd : Date -> String
dateYyMmDd date =
    let
        y =
            date |> Date.year |> toString

        m =
            date |> Date.month |> monthToInt |> pad

        d =
            date |> Date.day |> pad
    in
        y ++ "-" ++ m ++ "-" ++ d


pad : Int -> String
pad x =
    if x < 10 then
        "0" ++ (toString x)
    else
        (toString x)


monthToInt : Month -> Int
monthToInt month =
    case month of
        Date.Jan ->
            1

        Date.Feb ->
            2

        Date.Mar ->
            3

        Date.Apr ->
            4

        Date.May ->
            5

        Date.Jun ->
            6

        Date.Jul ->
            7

        Date.Aug ->
            8

        Date.Sep ->
            9

        Date.Oct ->
            10

        Date.Nov ->
            11

        Date.Dec ->
            12
