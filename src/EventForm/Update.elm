module EventForm.Update exposing (newEvent, update)

import Event exposing (Event)
import EventForm.Types exposing (..)


newEvent : Event
newEvent =
    { name = ""
    , dateStart = ""
    , dateEnd = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input Name value ->
            { model | name = value } ! []

        Input DateStart value ->
            { model | dateStart = value } ! []

        Input DateEnd value ->
            { model | dateEnd = value } ! []

        Submit ->
            Debug.crash "TODO: EventForm.update Submit"
