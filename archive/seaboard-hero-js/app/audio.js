'use strict';

import Voice        from 'voice';
import midi         from 'midi_message';

const Ctx = (window.AudioContext || window.webkitAudioContext);
const ctx = new Ctx();
const voices = {};

function stop(note) {
  const voice = voices[note];
  delete voices[note];
  voice.stop();
}

function play(note) {
  if (voices[note]) { return console.log(`Already playing ${note}`); }
  const hz = midi.noteToHz(note);
  const voice = new Voice(ctx, ctx.destination);
  voice.play(hz);
  voices[note] = voice;
}

export default { play, stop };
