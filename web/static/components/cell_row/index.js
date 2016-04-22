import React, { PropTypes } from "react";
import Cell from "../cell";

const CellRow = ({ cellStates, y }) => (
  <div className="row">
    <div className="row__info">
      Inst { y + 1 }
    </div>
    <div className="row__sequence">  
    {
      cellStates.map((state, index) => (
        <Cell key={index} state={state} x={index} y={y} />
      ))
    }
    </div>
  </div>
);

const cellStateType = PropTypes.oneOfType([
  PropTypes.string,
  PropTypes.bool,
]);

CellRow.propTypes = {
  cellStates: PropTypes.arrayOf(cellStateType).isRequired,
  y: PropTypes.number.isRequired,
};

export default CellRow;
