  import store      from "./store";
  import Synth      from "./synth";

  console.log("SEQUENCER LOADED");

  Synth.init();

  var soundBuffer = [];
  var pathName = "samples/TR808/";
  var sounds = [];

  sounds[0] = pathName + "kick-2.wav";
  sounds[1] = pathName + "snare-2.wav";
  sounds[2] = pathName + "closed-hat-2.wav";
  sounds[3] = pathName + "clap-2.wav";
  sounds[4] = pathName + "cowbell-2.wav";
  sounds[5] = pathName + "open-hat-2.wav";
  sounds[6] = pathName + "closed-hat-2.wav";
  sounds[7] = pathName + "shaker-suckup.wav";

  var LOOP_LENGTH = 16;

  var context;
  var compressor;
  var masterGainNode;

  var noteTime;
  var startTime;
  var lastDrawTime = -1;
  var rhythmIndex = 0;
  var timeoutId;

  var tempo = store.getState().bpm;

  if (window.hasOwnProperty('AudioContext')) {
    window.audioContext = AudioContext;
  }

  init();

  function init() {
    playPauseListener();
    initializeAudioNodes();
    loadSamples();
  }

  function playPauseListener() {
    document.getElementById('play-pause').addEventListener('click', function() {
      if(handlePlay.clicked) {
          handleStop();
        }else{
          handlePlay();
        }
    });
  }

  function loadSamples(){
    var request = [];
    for(var i=0;i<sounds.length;i++){
      (function(i) {
        request[i] = new XMLHttpRequest();
        request[i].open("GET", sounds[i], true);
        request[i].responseType = "arraybuffer";
        request[i].onload = function() {
          context.decodeAudioData(request[i].response, function(buffer){
            soundBuffer[i] = buffer;
          });
        }
        request[i].send();
      })(i);
    }
  }

  function initializeAudioNodes() {
    context = new audioContext();
    var finalMixNode;
    if (context.createDynamicsCompressor) {
        // Create a dynamics compressor to sweeten the overall mix.
        compressor = context.createDynamicsCompressor();
        compressor.connect(context.destination);
        finalMixNode = compressor;
    } else {
        // No compressor available in this implementation.
        finalMixNode = context.destination;
    }

    // Create master volume.
    masterGainNode = context.createGain();
    masterGainNode.gain.value = 0.7; // reduce overall volume to avoid clipping
    masterGainNode.connect(finalMixNode);

  }

  function playNote(buffer, noteTime) {
    if(buffer>7) {
      // Synth.playNotes([1,2,3]);
      return;
    }
    buffer = soundBuffer[buffer];
    var voice = context.createBufferSource();
    voice.buffer = buffer;

    var currentLastNode = masterGainNode;

    voice.connect(currentLastNode);
    voice.start(noteTime);
  }

  let step = 0;
  function schedule() {
    var currentTime = context.currentTime;

    // The sequence starts at startTime, so normalize currentTime so that it's 0 at the start of the sequence.
    currentTime -= startTime;

    while (noteTime < currentTime + 0.200) {
          const contextPlayTime = noteTime + startTime;
          const grid = store.getState().grid;
          grid.forEach((row, y) => {
            console.log('row', step);
            if (row[step]) { playNote(y, contextPlayTime); }
          });
          step = (step + 1) % grid[0].length;

        advanceNote();
    }

    timeoutId = requestAnimationFrame(schedule)
  }

  function advanceNote() {
      // Advance time by a 16th note...
      var secondsPerBeat = 60.0 / tempo;
      rhythmIndex++;
      if (rhythmIndex == LOOP_LENGTH) {
          rhythmIndex = 0;
      }

      noteTime += 0.25 * secondsPerBeat
  }

  function handlePlay(event) {
      //reset index to zero on start play
      rhythmIndex = 0;
      noteTime = 0.0;
      startTime = context.currentTime + 0.005;
      schedule();
      handlePlay.clicked = true;
      document.getElementById('button-text').innerHTML = 'STOP';
  }

  function handleStop(event) {
    cancelAnimationFrame(timeoutId);
    handlePlay.clicked = false;
    document.getElementById('button-text').innerHTML = 'START';

  }
