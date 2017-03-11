port module Update exposing (update)

import Types exposing (..)
import EventForm.Update as EventForm
import EventForm.Types


type alias Update =
    ( Model, Cmd Msg )


update : Msg -> Model -> Update
update msg model =
    case msg of
        LogOut ->
            model ! [ logOut () ]

        EventFormMsg formMsg ->
            eventFormUpdate formMsg model


eventFormUpdate : EventForm.Types.Msg -> Model -> Update
eventFormUpdate msg model =
    let
        ( event, cmd ) =
            EventForm.update msg model.newEvent
    in
        ( { model | newEvent = event }, Cmd.map EventFormMsg cmd )


{-| Defer to JS to log out
-}
port logOut : () -> Cmd msg
