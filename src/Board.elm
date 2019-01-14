module Board exposing (Board, Cell, new)

import Trope exposing (Trope(..))


type CellStatus
    = Checked
    | Unchecked


type alias Cell =
    { trope : Trope, status : CellStatus }


type alias Board =
    { cell0 : Cell
    , cell1 : Cell
    , cell2 : Cell
    , cell3 : Cell
    }


new : Board
new =
    { cell0 = newCell Elixir
    , cell1 = newCell Hummus
    , cell2 = newCell TankTop
    , cell3 = newCell Javascript
    }


newCell : Trope -> Cell
newCell trope =
    { trope = trope, status = Unchecked }
