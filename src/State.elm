port module State exposing (init, subscriptions, update)

import Array
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
            Debug.crash "TODO: Hit API to create new event"

        NewEventMsg formMsg ->
            eventFormUpdate NewEventMsg formMsg model



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


init : Flags -> ( Model, Cmd Msg )
init flags =
    { newEvent = EventForm.init
    , events = Array.fromList []
    }
        ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
