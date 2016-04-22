const initialState = 130;

function subject(state = initialState, action = undefined) {
  switch (action.type) {
    case "SET_BPM":
      return action.bpm;

    default:
      return state;
  }
}

export default subject;
