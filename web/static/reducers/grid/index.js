// const initialState = [[]];
const initialState = [
  [ false, false, false, false, false, false, true,  false ],
  [ false, false, true,  false, true,  false, false, false ],
  [ false, true,  false, false, true,  false, false, false ],
  [ true,  false, true,  false, true,  false, true,  false ],
  [ false, false, false, false, false, false, false, true  ],
  [ false, false, false, true,  false, false, true,  false ],
  [ false, false, false, false, false, false, true,  false ],
  [ true,  true,  false, false, false, false, true,  false ],
];

function subject(state = initialState, action = undefined) {
  switch (action.type) {
    case "SET_GRID":
      return action.grid;

    default:
      return state;
  }
}

export default subject;
