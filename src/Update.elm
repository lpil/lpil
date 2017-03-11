port module Update exposing (update)

import Types exposing (..)
import NewEvent.State as NewEvent


update : Msg -> Model -> Update
update msg model =
    case msg of
        LogOut ->
            model ! [ logOut () ]

        NewEventInput field value ->
            NewEvent.updateInput field value model

        NewEventSubmit ->
            NewEvent.updateSubmit model


{-| Defer to JS to log out
-}
port logOut : () -> Cmd msg
