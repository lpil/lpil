import { setGrid } from "../../actions";
import grid        from ".";

it("has default arguments", () => {
  const expected = [[]];
  const result = grid(undefined, {});
  expect(result).to.be.deepEqual(expected);
});

it("handles SET_MODIFIERS", () => {
  const grid   = [[1, 2], [3, 4]];
  const action = setModifiers(grid);
  const result = grid(undefined, action);
  expect(result).to.be.deepEqual(grid);
});
