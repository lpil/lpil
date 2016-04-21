import React, { PropTypes } from "react";

const handleClick = function handleClick(x) {
  console.log(x);
};

const Cell = ({ active, x }) => (
  <div
      className="cell"
      onClick={function() { handleClick(x); }}
      style={{ backgroundColor: active ? "hotpink" : "black" }}
  ></div>
);

Cell.propTypes = {
  active: PropTypes.bool.isRequired,
  x: PropTypes.number.isRequired,
};

export default Cell;
