import React, { PropTypes } from "react";
import { setCell }          from "../../network";

const Cell = ({ active, x, y }) => (
  <div
      className="cell"
      onClick={function() { setCell(x, y, !active); }}
  >
    <div 
        className="cell__square"
        style={{ backgroundColor: active ? "hotpink" : "black" }}
    >
    </div>
  </div>
);

Cell.propTypes = {
  active: PropTypes.bool.isRequired,
  x: PropTypes.number.isRequired,
  y: PropTypes.number.isRequired,
};

export default Cell;
