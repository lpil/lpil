module Update exposing (update)

import Array
import Types exposing (..)
import NewEvent.Types exposing (..)
import NewEvent.State as NewEvent


type alias Update =
    ( Model, Cmd Msg )


update : Msg -> Model -> Update
update msg model =
    case msg of
        NewEventInput field value ->
            newEventInput field value model

        NewEventSubmit ->
            newEventSubmit model


{-| New event form has has new content entered.
-}
newEventInput : EventField -> String -> Model -> Update
newEventInput field value model =
    let
        event =
            NewEvent.updateField model.newEvent field value
    in
        { model | newEvent = event } ! []


{-| New event form has has been submitted
-}
newEventSubmit : Model -> Update
newEventSubmit model =
    { model
        | newEvent = NewEvent.newEvent
        , events = Array.push model.newEvent model.events
    }
        ! []
