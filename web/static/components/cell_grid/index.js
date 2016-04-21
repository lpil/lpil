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

const boolArray = PropTypes.arrayOf(PropTypes.bool);

CellGrid.propTypes = {
  rowStates: PropTypes.arrayOf(boolArray).isRequired,
};


export default CellGrid;
