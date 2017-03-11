module NewEvent.State exposing (newEvent, updateField)

import Types exposing (..)
import NewEvent.Types exposing (..)


newEvent : Event
newEvent =
    { name = ""
    , dateStart = ""
    , dateEnd = ""
    }


updateField : Event -> EventField -> String -> Event
updateField event field value =
    case field of
        Name ->
            { event | name = value }

        EventStart ->
            { event | dateStart = value }

        EventEnd ->
            { event | dateEnd = value }
