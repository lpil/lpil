module Accumulate exposing (accumulate)

import List


accumulate : (a -> b) -> List a -> List b
accumulate fun list =
    iter fun list []
        |> List.reverse


iter : (a -> b) -> List a -> List b -> List b
iter fun list acc =
    case list of
        [] ->
            acc

        _ ->
            iter fun (tail list) ((list |> head |> fun) :: acc)


tail : List a -> List a
tail list =
    case List.tail list of
        Just aTail ->
            aTail

        Nothing ->
            []


head : List a -> a
head list =
    case List.head list of
        Just aHead ->
            aHead

        Nothing ->
            Debug.crash "head of empty list"
