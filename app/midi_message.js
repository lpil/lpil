'use strict';

function noteToHz(note) {
  return 440.0 * Math.pow(2, (note - 69.0) / 12.0);
}

function msgToHz(msg) {
  return noteToHz(msg.data[1]);
}

export default { noteToHz, msgToHz };
