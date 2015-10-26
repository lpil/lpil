'use strict';

import audio from 'audio';

const TYPE_ON  = 144;
const TYPE_OFF = 128;

function onMessage(msg) {
  const data = msg.data;
  const type = data[0] & 0xf0;
  // const channel = data[0] & 0xf;

  if (type === TYPE_ON) {
    audio.play(data[1]);
  } else if (type === TYPE_OFF) {
    audio.stop(data[1]);
  }
}

export default { onMessage };
