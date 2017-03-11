module NewEvent.State
    exposing
        ( newEvent
        , updateField
        , updateSubmit
        , updateInput
        )

import Array
import Types exposing (..)
import NewEvent.Types exposing (..)


newEvent : Event
newEvent =
    { name = ""
    , dateStart = ""
    , dateEnd = ""
    }


{-| New event form has has new content entered.
-}
updateInput : EventField -> String -> Model -> Update
updateInput field value model =
    let
        event =
            updateField model.newEvent field value
    in
        { model | newEvent = event } ! []


{-| New event form has has been submitted
-}
updateSubmit : Model -> Update
updateSubmit model =
    { model
        | newEvent = newEvent
        , events = Array.push model.newEvent model.events
    }
        ! []


updateField : Event -> EventField -> String -> Event
updateField event field value =
    case field of
        Name ->
            { event | name = value }

        EventStart ->
            { event | dateStart = value }

        EventEnd ->
            { event | dateEnd = value }
