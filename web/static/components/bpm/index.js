import React, { PropTypes } from "react";

// <div className="meta__button">Play / Stop</div>

function foo(x) {
  console.log(x);
}

const BPM = ({ currentBPM }) => (
  <div className="meta__controls">
    <div className="meta__bpm">BPM - {currentBPM}</div>
    <div className="meta__button meta__button--half">+</div>
    <div className="meta__button meta__button--half">-</div>
  </div>
);

BPM.propTypes = {
  currentBPM: PropTypes.number.isRequired,
};


export default BPM;
