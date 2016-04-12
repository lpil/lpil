import "./css/app.scss";

import "phoenix_html";
import network from "./network";

window.network = network;

import "./reducers";

import React    from "react";
import ReactDOM from "react-dom";
import CellGrid from "./components/cell_grid";

const rowStates = [
  [ false, false, false, false, false, false, true,  false ],
  [ false, false, true,  false, true,  false, false, false ],
  [ false, true,  false, false, true,  false, false, false ],
  [ true,  false, true,  false, true,  false, true,  false ],
  [ false, false, false, false, false, false, false, true  ],
  [ false, false, false, true,  false, false, true,  false ],
  [ false, false, false, false, false, false, true,  false ],
  [ true,  true,  false, false, false, false, true,  false ],
];

ReactDOM.render(
  <CellGrid rowStates={rowStates} />,
  document.querySelector("#app")
);
