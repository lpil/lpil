import React, { PropTypes } from "react";
import Cell from "../cell";

const CellRow = ({ cellStates, y }) => (
  <div>
  {
    cellStates.map((active, index) => (
      <Cell x={index} y={y} active={active} key={index} />
    ))
  }
  </div>
);

CellRow.propTypes = {
  cellStates: PropTypes.arrayOf(PropTypes.bool).isRequired,
  y: PropTypes.number.isRequired,
};

export default CellRow;
