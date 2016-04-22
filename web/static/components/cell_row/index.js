import React, { PropTypes } from "react";
import Cell from "../cell";

const CellRow = ({ cellStates, y }) => (
  <div className="row">
  {
    cellStates.map((state, index) => (
      <Cell key={index} state={state} x={index} y={y} />
    ))
  }
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
