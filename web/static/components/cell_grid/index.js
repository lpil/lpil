import React, { PropTypes } from "react";
import CellRow from "../cell_row";

const CellGrid = ({ rowStates }) => (
  <div>
  {
    rowStates.map((cellStates, index) =>
      <CellRow cellStates={cellStates} y={index} key={index} />
    )
  }
  </div>
);

CellGrid.propTypes = {
  rowStates: PropTypes.arrayOf(PropTypes.array).isRequired,
};


export default CellGrid;
