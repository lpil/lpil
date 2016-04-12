import { setGrid } from "../../actions";
import grid        from ".";

it("has default arguments", () => {
  const row      = new Array(8).fill(false);
  const expected = new Array(8).fill(row);
  const result = grid(undefined, {});
  expect(result).to.be.deepEqual(expected);
});

it("handles SET_MODIFIERS", () => {
  const grid   = [[1, 2], [3, 4]];
  const action = setModifiers(grid);
  const result = grid(undefined, action);
  expect(result).to.be.deepEqual(grid);
});
