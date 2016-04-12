import React, { PropTypes } from "react";

const Cell = ({ active }) => (
  <div
    style={{ backgroundColor: active ? "hotpink" : "black" }}
    className="cell"
  ></div>
);

Cell.propTypes = {
  active: PropTypes.bool.isRequired,
};

export default Cell;
