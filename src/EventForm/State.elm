module EventForm.State exposing (init, update)

import Event exposing (Event)
import EventForm.Types exposing (..)


init : Model
init =
    { name = ""
    , url = ""
    , dateStart = ""
    , dateEnd = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input Name value ->
            { model | name = value } ! []

        Input Url value ->
            { model | url = value } ! []

        Input DateStart value ->
            { model | dateStart = value } ! []

        Input DateEnd value ->
            { model | dateEnd = value } ! []

        Submit ->
            Debug.log "Handle Submit at a higher level!" model ! []
