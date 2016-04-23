  import store      from "./store";
  import Synth      from "./synth";

  console.log("SEQUENCER LOADED - READY TO GO");
  Synth.init();
  //KITS start
  var NUM_INSTRUMENTS = 2;
  var soundKit = [];
  function Kit(name) {
    this.SAMPLE_BASE_PATH = "samples/";
    this.name = name;

    this.kickBuffer = null;
    this.snareBuffer = null;
    this.hihatBuffer = null;

    this.startedLoading = false;
    this.isLoaded = false;
    this.instrumentLoadCount = 0;
  }

  Kit.prototype.pathName = function() {
    return this.SAMPLE_BASE_PATH + this.name + "/";
  };

  Kit.prototype.load = function() {
    if (this.startedLoading) {
      return;
    }

    this.startedLoading = true;

    var pathName = this.pathName();

    var kickPath = pathName + "kick.mp3";
    var snarePath = pathName + "snare.mp3";
    var hihatPath = pathName + "hihat.mp3";
    var clapPath = pathName + "clap-2.wav";
    var cowbellPath = pathName + "cowbell-2.wav";
    var openhatPath = pathName + "open-hat-2.wav";
    var closedhatPath = pathName + "closed-hat-2.wav";
    var shakerPath = pathName + "shaker-suckup.wav";

    this.loadSample(kickPath, "kick");
    this.loadSample(snarePath, "snare");
    this.loadSample(hihatPath, "hihat");
    this.loadSample(clapPath, "clap");
    this.loadSample(cowbellPath, "cowbell");
    this.loadSample(openhatPath, "openhat");
    this.loadSample(closedhatPath, "closedhat");
    this.loadSample(shakerPath, "shaker");
  };

  Kit.prototype.loadSample = function(url, instrumentName) {
    //need 2 load asynchronously
    var request = new XMLHttpRequest();
    request.open("GET", url, true);
    request.responseType = "arraybuffer";

    var kit = this;

    request.onload = function() {
      context.decodeAudioData(
        request.response,
        function(buffer) {
          switch (instrumentName) {
            case "kick":
              kit.kickBuffer = buffer;
              soundKit[0] = buffer;
              break;
            case "snare":
              kit.snareBuffer = buffer;
              soundKit[1] = buffer;
              break;
            case "hihat":
              kit.hihatBuffer = buffer;
              soundKit[2] = buffer;
              break;
            case "clap":
              kit.clapBuffer = buffer;
              soundKit[3] = buffer;
              break;
            case "cowbell":
              kit.cowbellBuffer = buffer;
              soundKit[4] = buffer;
              break;
            case "openhat":
              kit.openhatBuffer = buffer;
              soundKit[5] = buffer;
              break;
            case "closedhat":
              kit.closedhatBuffer = buffer;
              soundKit[6] = buffer;
              break;
            case "shaker":
              kit.shakerBuffer = buffer;
              soundKit[7] = buffer;
              break;
          }
          kit.instrumentLoadCount++;
          if (kit.instrumentLoadCount === NUM_INSTRUMENTS) {
            kit.isLoaded = true;
          }
        },
        function(buffer) {
          console.log("Error decoding drum samples!");
        }
      );
    }
    request.send();
  }

  //KITS END

  //SEQUENCER START

  //audio node variables
  var context;
  var compressor;
  var masterGainNode;
  var effectLevelNode;

  var noteTime;
  var startTime;
  var lastDrawTime = -1;
  var LOOP_LENGTH = 16;
  var rhythmIndex = 0;
  var timeoutId;
  var testBuffer = null;

  var currentKit = null;
  var reverbImpulseResponse = null;

  var tempo = store.getState().bpm;

  if (window.hasOwnProperty('AudioContext')) {
    window.audioContext = AudioContext;
  }

    init();
    toggleSelectedListener();
    playPauseListener();

  function changeFrequency(event, ui) {
    var minValue = 40;
    var maxValue = context.sampleRate / 2;
    var numberOfOctaves = Math.log(maxValue / minValue) / Math.LN2;
    var multiplier = Math.pow(2, numberOfOctaves * (ui.value - 1.0));
    lowPassFilterNode.frequency.value = maxValue * multiplier;
  }

  function changeQuality(event, ui) {
    //30 is the quality multiplier, for now.
    lowPassFilterNode.Q.value = ui.value * 30;
  }

  function playPauseListener() {
    console.log('PLAY PAUSE',document.getElementById('play-pause'));

    document.getElementById('play-pause').addEventListener('click', function() {
      console.log('BUTTON PUSHED');
      if(handlePlay.clicked) {
          handleStop();
        }else{
          handlePlay();
        }
    });
  }

  function toggleSelectedListener() {
    var cells = document.getElementsByClassName("cell");
    var myFunction = function() {
      this.toggleClass("selected");
    };
    for (var i = 0; i < cells.length; i++) {
        cells[i].addEventListener('click', myFunction, false);
    }
  }

  function init() {
    initializeAudioNodes();
    loadKits();
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

  function loadKits() {
    //name must be same as path
    var kit = new Kit("TR808");
    kit.load();

    currentKit = kit;
  }

  function playNote(buffer, noteTime) {
    if(buffer>7) {
      // Synth.playNotes([1,2,3]);
      return;
    }
    buffer = soundKit[buffer];
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

      //0.25 because each square is a 16th note
      noteTime += 0.25 * secondsPerBeat
  }

  function handlePlay(event) {
      rhythmIndex = 0;
      noteTime = 0.0;
      startTime = context.currentTime + 0.005;
      schedule();
      handlePlay.clicked = true;
  }

  function handleStop(event) {
    cancelAnimationFrame(timeoutId);
    handlePlay.clicked = false;
  }


  //SEQUENCER END
