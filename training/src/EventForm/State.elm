module EventForm.State exposing (init, update)

import Array
import EventForm.Types exposing (..)


init : Model
init =
    { name = ""
    , url = ""
    , interestedPeople = Array.empty
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

        Input (InterestedPeople i) value ->
            updateInterestedPeople model i value ! []

        Submit ->
            Debug.log "Handle Submit at a higher level!" model ! []


updateInterestedPeople : Model -> Int -> String -> Model
updateInterestedPeople model index name =
    let
        people =
            if index >= Array.length model.interestedPeople then
                Array.push name model.interestedPeople
            else
                Array.set index name model.interestedPeople

        filtered =
            Array.filter (\x -> x == "") people
    in
        { model | interestedPeople = filtered }
