import React from "react";

const Meta = () => (
  <div className="meta">
    <h1 className="meta__title">
      Block Party
    </h1>
    <div className="meta__controls">
      <div className="meta__button">
        Play / Stop
      </div>
      <div className="meta__bpm">
        BPM - 124
      </div>

      <div className="meta__button meta__button--half">
        +
      </div>
      <div className="meta__button meta__button--half">
        -
      </div>
    </div>
  </div>
);

export default Meta;
