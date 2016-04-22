import React, { PropTypes } from "react";
import Cell from "../cell";

const CellRow = ({ cellStates, y }) => (
  <div>
  {
    cellStates.map((state, index) => (
      <Cell x={index} y={y} state={state} key={index} />
    ))
  }
  </div>
);

CellRow.propTypes = {
  cellStates: PropTypes.array.isRequired,
  y: PropTypes.number.isRequired,
};

export default CellRow;
