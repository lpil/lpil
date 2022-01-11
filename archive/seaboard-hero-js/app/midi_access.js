'use strict';

function findDevice(devices, nameRegex) {
  var result = devices.next();
  while (!result.done) {
    if (nameRegex.test(result.value.name)) { return result.value; }
    result = devices.next();
  }
}

// Callback gets the input and output as arguments
// Name should be a regex
//
function getDevice(cb, nameRegex) {
  navigator.requestMIDIAccess().then(
    (midiAccess) => {
      const inputs  = midiAccess.inputs.values();
      const outputs = midiAccess.outputs.values();
      const input   = findDevice(inputs, nameRegex);
      const output  = findDevice(outputs, nameRegex);
      cb(input, output);
    },
    ( ) => { throw new Error('Failed to get midi access.'); }
  );

}

export default { getDevice };
