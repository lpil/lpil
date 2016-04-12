import React, { PropTypes } from "react";
import Cell from "../cell";

const CellRow = ({ cellStates }) => (
  <div>
  {
    cellStates.map((active, index) => <Cell active={active} key={index} />)
  }
  </div>
);

CellRow.propTypes = {
  cellStates: PropTypes.arrayOf(PropTypes.bool).isRequired,
};

export default CellRow;
