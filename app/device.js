'use strict';

import midiAccess from './midi_access';

const device = {};

function registerRISE(controller, input, output) {
  if (input && output) {
    device.input  = input;
    device.output = output;
    input.onmidimessage = controller.onMessage;
    console.log('Registered RISE');
  } else {
    throw 'ERROR: Launchpad MIDI device not found';
  }
}

function initDevice(controller) {
  midiAccess.getDevice(
    (i, o) => registerRISE(controller, i, o),
    /^Seaboard RISE$/
  );
}

export default initDevice;
