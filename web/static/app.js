import "./css/app.scss";
import "phoenix_html";

import reducer         from "./reducers";
import { createStore } from "redux";

const store = createStore(reducer);
console.log(store.getState());

import React        from "react";
import ReactDOM     from "react-dom";
import { Provider } from "react-redux";
import Grid         from "./containers/grid";

ReactDOM.render(
  <Provider store={store}><Grid /></Provider>,
  document.querySelector("#app")
);
