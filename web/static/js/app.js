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

channel.on("msg", payload => { console.log("msg: ", payload); });


function transmit(x) {
  channel.push("new_msg", { body: x });
}
window.transmit = transmit;
