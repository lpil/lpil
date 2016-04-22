import "./css/app.scss";
import "phoenix_html";

import reducer         from "./reducers";
import { createStore } from "redux";

const store = createStore(reducer);
console.log(store.getState());

import React        from "react";
import ReactDOM     from "react-dom";
import { Provider } from "react-redux";
import Sequencer    from "./components/sequencer"

import setGrid           from "./action_creators/set_grid";
import { subscribeGrid } from "./network";
const boundSetGrid = (grid) => store.dispatch(setGrid(grid));
subscribeGrid(boundSetGrid);

ReactDOM.render(
  <Provider store={store}><Sequencer /></Provider>,
  document.querySelector("#app")
);
