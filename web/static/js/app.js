import "../css/app.scss";

import "phoenix_html";
import { Socket } from "phoenix";

const socket = new Socket("/socket", { params: {} });

socket.connect();

const channel = socket.channel("rooms:lobby", {});
channel
  .join()
  .receive("ok",    resp => { console.log("Joined successfully", resp); })
  .receive("error", resp => { console.log("Unable to join", resp); });

channel.on("set_cell", payload => {
  console.log("Server sent set_cell:", payload);
});


function setCell(x, y, state) {
  const params = { x, y, state };
  console.log("Sending set_cell:", params);
  channel.push("set_cell", params);
}
window.setCell = setCell;
