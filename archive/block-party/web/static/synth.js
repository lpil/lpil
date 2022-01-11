  var channels = 8;
  var defaultModeName = "dorian";
  var currentModeName;
  var currentMode = [];
  var previousNotes = [];
  var modes = {
    ionian:     ["C4","D4","E4","F4","G4","A4","B4","C5"],
    dorian:     ["D3","E3","F3","G3","A3","B3","C4","D4"],
    phrygian:   ["E3","F3","G3","A3","B3","C4","D4","E4"],
    lydian:     ["F3","G3","A3","B3","C4","D4","E4","F4"],
    mixolydian: ["G3","A3","B3","C4","D4","E4","F4","G4"],
    aeolian:    ["A3","B3","C4","D4","E4","F4","G4","A4"],
    locrian:    ["B3","C4","D4","E4","F4","G4","A4","B4"],
  };
  // concert pitch in HZ
  var frequencies = {
    C3: 130.813,
    D3: 146.832,
    E3: 164.814,
    F3: 174.614,
    G3: 195.998,
    A3: 220,
    B3: 246.942,
    C4: 261.626,
    D4: 293.665,
    E4: 329.628,
    F4: 349.228,
    G4: 391.995,
    A4: 440,
    B4: 493.883,
    C5: 523.251,
  };
  var types = [ "sine","square","sawtooth","triangle" ];
  var context;
  var oscillators = [];

  function init() {
    console.log("SYNTH LOADED");
    // normalize and create a new AudioContext if supported
    window.AudioContext = window.AudioContext || window.webkitAudioContext;

    if ("AudioContext" in window) {
      context = new AudioContext();
      for (var i = channels - 1; i >= 0; i--) {
        // contexts[i] = new AudioContext();
        oscillators[i] = context.createOscillator();
        oscillators[i].connect(context.destination);
        oscillators[i].frequency.value = 0;
        oscillators[i].frequency.value = 0;
        oscillators[i].start();
      }
      setMode(defaultModeName);
    } else {
      throw new Error("synth.js: browser does not support Web Audio API");
    }
  }

  function setMode(name){
    if (modes[name] !== undefined) {
      for (var i = modes[name].length - 1; i >= 0; i--) {
        // convert to frequency
        currentMode[i] = frequencies[ modes[name][i] ];
      }
      playNotes(previousNotes);
    }
  }

  // Expects an array of 8 values from 0-4
  // 0 - don't play this note (can also be false/null/undefined)
  // 1..4 - play this note using waveform types:
  // 1 - sine
  // 2 - square
  // 3 - sawtooth
  // 4 - triangle
  function playNotes(notes) {
    // save for reference
    previousNotes = notes;
    // iterate through the notes
    for (var i = 0; i < notes.length; i++) {
      // TODO stop the oscillator if note is null
      // TODO keep it going if it's the same
      // TODO set new otherwise
      if (notes[i] && currentMode[i]) {
        oscillators[i].frequency.value = currentMode[i];
        oscillators[i].type = types[ notes[i]-1 ];
      } else {
        oscillators[i].frequency.value = 0;
      }

    }
  }

  function releaseNotes() {
    for (var i = 0; i < oscillators.length; i++) {
      oscillators[i].frequency.value = 0;
    }
  }

  function getModeNames() {
    return Object.keys(modes);
  }

  function getCurrentMode() {
    return currentMode;
  }

  function getCurrentModeName() {
    return currentModeName;
  }

	// returned functions
  var Synth = {
    init: init,
    getModeNames: getModeNames,
    setMode: setMode,
    getCurrentModeName: getCurrentModeName,
    getCurrentMode: getCurrentMode,
    playNotes: playNotes,
    releaseNotes: releaseNotes,
    STFU: releaseNotes,
  };

  export default Synth;
