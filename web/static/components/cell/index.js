import React, { PropTypes } from "react";

const handleClick = function handleClick(x, y) {
  console.log(x, y);
};

const Cell = ({ active, x, y }) => (
  <div
      className="cell"
      onClick={function() { handleClick(x, y); }}
      style={{ backgroundColor: active ? "hotpink" : "black" }}
  ></div>
);

Cell.propTypes = {
  active: PropTypes.bool.isRequired,
  x: PropTypes.number.isRequired,
  y: PropTypes.number.isRequired,
};

export default Cell;
