port module State exposing (update)

import Types exposing (..)
import EventForm.State as EventForm
import EventForm.Types as EventFormT


type alias Update =
    ( Model, Cmd Msg )


update : Msg -> Model -> Update
update msg model =
    case msg of
        LogOut ->
            model ! [ logOut () ]

        NewEventMsg (EventFormT.Submit) ->
            model ! [ model.createEvent model.newEvent ]

        NewEventMsg formMsg ->
            eventFormUpdate NewEventMsg formMsg model

        FailResponse error ->
            Debug.crash ("TODO: update FailResponse" ++ (toString error))

        CreateEventResponse value ->
            let
                _ =
                    Debug.log "CreateEventResponse" value
            in
                Debug.crash "TODO: update CreateEventResponse"

        CreateUserResponse value ->
            model ! [ model.fetchEvents ]

        FetchEventsResponse events ->
            { model | events = events } ! []



-- Helpers


eventFormUpdate : (EventFormT.Msg -> Msg) -> EventFormT.Msg -> Model -> Update
eventFormUpdate msgConstructor msg model =
    let
        ( event, formCmd ) =
            EventForm.update msg model.newEvent

        cmd =
            Cmd.map msgConstructor formCmd
    in
        ( { model | newEvent = event }, cmd )


{-| Defer to JS to log out
-}
port logOut : () -> Cmd msg
