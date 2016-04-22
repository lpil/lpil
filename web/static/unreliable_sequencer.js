import { play } from "./sampler";
import store    from "./store";

let step = 0;

function tick() {
  const grid = store.getState().grid;
  grid.forEach((row, x) => {
    if (row[step]) { play(x) }
  });
  step = (step + 1) % grid[0].length;
}

setInterval(tick, 200);
