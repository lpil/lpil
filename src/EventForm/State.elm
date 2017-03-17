module EventForm.State exposing (init, update)

import Event exposing (Event)
import EventForm.Types exposing (..)


init : Model
init =
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
            Debug.log "Handle Submit at a higher level!" model ! []
