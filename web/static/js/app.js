import "../css/app.scss";

import "phoenix_html";
import { Socket } from "phoenix";

const socket = new Socket("/socket", { params: {} });

socket.connect();

const channel = socket.channel("sequencers:lobby", {});
channel
  .join()
  .receive("ok",    resp => { console.log("Joined successfully", resp); })
  .receive("error", resp => { console.log("Unable to join", resp); });

channel.on("grid", payload => {
  console.log("Server sent grid:", payload);
});


function setCell(x, y, active) {
  const params = { x, y, active };
  console.log("Sending set_cell:", params);
  channel.push("set_cell", params);
}
window.setCell = setCell;
