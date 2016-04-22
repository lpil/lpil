import { Socket } from "phoenix";
import setGrid    from "./action_creators/set_grid";
import store      from "./store";

const topic  = "sequencers:lobby";
const socket = new Socket("/socket", { params: {} });

socket.connect();

const channel = socket.channel(topic, {});
channel
  .join()
  .receive("ok",    resp => { console.log("Joined successfully", resp); })
  .receive("error", resp => { console.log("Unable to join", resp); });

channel.on("grid", payload => {
  console.log("[IN grid]:", payload);
  const grid   = payload.grid;
  const action = setGrid(grid);
  store.dispatch(action);
});

//
// Broadcast whether a cell is on or off to the network.
// `active` must be a boolean.
//
function setCell(x, y, active) {
  const params = { x, y, active };
  console.log("[OUT set_cell]:", params);
  channel.push("set_cell", params);
}

export { setCell };
