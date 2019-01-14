module App exposing (main)

import View
import State
import Html.App


main : Program Never
main =
    Html.App.program
        { init = State.init
        , update = State.update
        , view = View.root
        , subscriptions = subscriptions
        }



-- Subscriptions


subscriptions : State.Model -> Sub a
subscriptions model =
    Sub.none
