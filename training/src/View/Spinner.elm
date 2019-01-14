module View.Spinner exposing (root)

import Html exposing (Html)
import Svg
import Svg.Attributes exposing (..)


root : Bool -> Html a
root visible =
    let
        className =
            if visible then
                "hexagon-spinner"
            else
                "hexagon-spinner hidden"

        dValue =
            "M253.91,49.72a5.45,5.45,0,0,1,3.16,3.76l25.31,142.8a5.45,5.45,0,0,1-1.68,4.62l-111,93.33a5.45,5.45,0,0,1-4.83.86L28.52,245.6a5.45,5.45,0,0,1-3.16-3.76L0.06,99a5.45,5.45,0,0,1,1.68-4.62l111-93.32a5.45,5.45,0,0,1,4.83-.86Z"
    in
        Svg.svg
            [ class className, viewBox "0 0 282.43 295.32" ]
            [ Svg.path [ d dValue ] [] ]
