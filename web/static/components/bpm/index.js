import React, { PropTypes } from "react";
import { addBPM }           from "../../network";

function incBPM() { addBPM(+5); }
function decBPM() { addBPM(-5); }

window.incBPM = incBPM;
window.incBPM = incBPM;

// <div className="meta__button">Play / Stop</div>

const BPM = ({ currentBPM, decBPM, incBPM }) => (
  <div className="meta__controls">
    <div className="meta__bpm">BPM - {currentBPM}</div>
    <div className="meta__button meta__button--half" onClick={incBPM}>+</div>
    <div className="meta__button meta__button--half" onClick={decBPM}>-</div>
  </div>
);

BPM.propTypes = {
  currentBPM: PropTypes.number.isRequired,
};


export default BPM;
