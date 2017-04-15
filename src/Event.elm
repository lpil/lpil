module Event exposing (Event)

import Array exposing (Array)


type alias Event =
    { name : String
    , url : String
    , dateStart : String
    , dateEnd : String
    , interestedPeople : Array String
    }
