import "./css/app.scss";
import "phoenix_html";

import React        from "react";
import ReactDOM     from "react-dom";
import { Provider } from "react-redux";
import store        from "./store";
import Sequencer    from "./components/sequencer";

import setGrid           from "./action_creators/set_grid";
import { subscribeGrid } from "./network";

import { play } from "./sampler";
window.play = play;

const boundSetGrid = (grid) => store.dispatch(setGrid(grid));
subscribeGrid(boundSetGrid);

ReactDOM.render(
  <Provider store={store}><Sequencer /></Provider>,
  document.querySelector("#app")
);
