import { SET_GRID } from "../../constants/action_types";

const initialState = "";

function subject(state = initialState, action = undefined) {
  switch (action.type) {
    case SET_GRID:
      return action.grid;

    default:
      return state;
  }
}

export default subject;
