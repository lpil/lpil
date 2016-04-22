import React, { PropTypes } from "react";
import CellRow from "../cell_row";

const CellGrid = ({ rowStates }) => (
  <div className="grid">
  {
    rowStates.map((cellStates, index) =>
      <CellRow cellStates={cellStates} key={index} y={index} />
    )
  }
  </div>
);

CellGrid.propTypes = {
  rowStates: PropTypes.arrayOf(
    PropTypes.arrayOf(
      PropTypes.oneOfType([PropTypes.string, PropTypes.bool])
    )
  ).isRequired,
};


export default CellGrid;
