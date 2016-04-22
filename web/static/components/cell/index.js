import React, { PropTypes } from "react";
import { setCell }          from "../../network";
import color                from "../../color";

function handleClick(x, y, currentState) {
  const newState = currentState ? false : color;
  setCell(x, y, newState);
}

const Cell = ({ state, x, y }) => (
  <div
      className="cell"
      onClick={function() { handleClick(x, y, state); }}
  >
    <div 
      className="cell__circle"
      style={{ backgroundColor: state || "black" }}
    >
    </div>
  </div>
);

Cell.propTypes = {
  state: PropTypes.oneOfType([
    PropTypes.string,
    PropTypes.bool,
  ]).isRequired,
  x: PropTypes.number.isRequired,
  y: PropTypes.number.isRequired,
};

export default Cell;
