import { Socket } from "phoenix";

const topic  = "sequencers:lobby";
const socket = new Socket("/socket", { params: {} });
let subscribers = [];

socket.connect();

const channel = socket.channel(topic, {});
channel
  .join()
  .receive("ok",    resp => { console.log("Joined successfully", resp); })
  .receive("error", resp => { console.log("Unable to join", resp); });

channel.on("grid", payload => {
  console.log("[IN grid]:", payload);
  subscribers.forEach(cb => { cb(payload.grid); });
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

//
// Subscribe a callback to changes in grid state passed from the network.
// `cb` receives a 2D array of booleans.
// Returns a function that can be used to unsubscribe
//
function subscribeGrid(cb) {
  subscribers.push(cb);
  return () => {
    const index = subscribers.indexOf(cb);
    subscribers.splice(index, 1);
  };
}

export { setCell, subscribeGrid };
