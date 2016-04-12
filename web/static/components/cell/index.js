import React, { PropTypes } from "react";

const Cell = ({ active }) => (
  <div
      className="cell"
      style={{ backgroundColor: active ? "hotpink" : "black" }}
  ></div>
);

Cell.propTypes = {
  active: PropTypes.bool.isRequired,
};

export default Cell;
