var Synth = (function () {

  var channels = 8;
	var defaultModeName = 'dorian';
	var currentModeName;
  var currentMode = [];
  var previousNotes = [];
	var modes = {
		// CDEFGABC
		ionian: ['C4','D4','E4','F4','G4','A4','B4','C5'],
		// DEFGABCD
		dorian: ['D3','E3','F3','G3','A3','B3','C4','D4'],
		// EFGABCDE
		phrygian: ['E3','F3','G3','A3','B3','C4','D4','E4'],
		// FGABCDEF
		lydian: ['F3','G3','A3','B3','C4','D4','E4','F4'],
		// GABCDEFG
		mixolydian: ['G3','A3','B3','C4','D4','E4','F4','G4'],
		// ABCDEFGA
		aeolian: ['A3','B3','C4','D4','E4','F4','G4','A4'],
		// BCDEFGAB
		locrian: ['B3','C4','D4','E4','F4','G4','A4','B4']
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
    C5: 523.251
  };  
  var context;
  var oscillators = {};

  function init() {
    // normalize and create a new AudioContext if supported
    window.AudioContext = window.AudioContext || window.webkitAudioContext;

    if ('AudioContext' in window) {
      context = new AudioContext();
      for (var i = channels - 1; i >= 0; i--) {
        console.log(i);
        // contexts[i] = new AudioContext();
        oscillators[i] = context.createOscillator();
        oscillators[i].connect(context.destination);
        oscillators[i].frequency.value = 0;
        oscillators[i].start();
        console.log(oscillators[i]);
      };
      setMode(defaultModeName);
    } else {
        throw new Error('synth.js: browser does not support Web Audio API');
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

  // expects an array of 8 values
  function playNotes(notes) {
    // save for reference
    previousNotes = notes;
    // iterate through the notes
    for (var i = 0; i < notes.length; i++) {
      // TODO stop the oscillator if note is null
      // TODO keep it going if it's the same
      // TODO set new otherwise
      //oscillators[i].stop();
      if (notes[i] && currentMode[i]) {
        oscillators[i].frequency.value = currentMode[i];
        //oscillators[i].start();
      } else {
        oscillators[i].frequency.value = 0;
      }
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
  return {
    init: init,
  	getModeNames: getModeNames,
  	setMode: setMode,
  	getCurrentModeName: getCurrentModeName,
  	getCurrentMode: getCurrentMode,
  	playNotes: playNotes
  };
	
})();