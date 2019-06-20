import { combineReducers } from "redux";
import grid from "./grid";
import bpm  from "./bpm";

const rootReducer = combineReducers({
  grid,
  bpm,
});

export default rootReducer;
