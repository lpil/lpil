import "./css/app.scss";
import "phoenix_html";

import React        from "react";
import ReactDOM     from "react-dom";
import { Provider } from "react-redux";
import store        from "./store";
import Sequencer    from "./components/sequencer";
import SeqFunctions from "./sequencer";

store.subscribe(() => console.log('state', store.getState()));

ReactDOM.render(
  <Provider store={store}><Sequencer /></Provider>,
  document.querySelector("#app")
);
