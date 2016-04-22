import "./css/app.scss";
import "phoenix_html";

import React        from "react";
import ReactDOM     from "react-dom";
import { Provider } from "react-redux";
import store        from "./store";
import Grid         from "./containers/grid";


ReactDOM.render(
  <Provider store={store}><Grid /></Provider>,
  document.querySelector("#app")
);
