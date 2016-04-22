  import store      from "./store";
  console.log("SEQUENCER LOADED - READY TO GO");
  import synth      from "./synth";
  synth.init();
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

    //don't want to have set number of instruments, or whatever
    var kickPath = pathName + "kick.mp3";
    var snarePath = pathName + "snare.mp3";
    var hihatPath = pathName + "hihat.mp3";
    var clapPath = pathName + "clap-2.wav";
    var cowbellPath = pathName + "cowbell-2.wav";
    var openhatPath = pathName + "open-hat-2.wav";
    var closedhatPath = pathName + "closed-hat-2.wav";
    var mysteryPath = pathName + "shaker-suckup.wav";

    this.loadSample(kickPath, "kick");
    this.loadSample(snarePath, "snare");
    this.loadSample(hihatPath, "hihat");
    this.loadSample(clapPath, "clap");
    this.loadSample(cowbellPath, "cowbell");
    this.loadSample(openhatPath, "openhat");
    this.loadSample(closedhatPath, "closedhat");
    this.loadSample(mysteryPath, "mystery");
  };

  //also make a class per buffer/sample? can store prettified name?

  //this should definitely be part of a sample class, pass in kit or st
  //if we have the name of a sample type, then we can do metaprogramming awesomeness.
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
            case "mystery":
              kit.mysteryBuffer = buffer;
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
    };
    request.send();
  };

  //KITS END

  //IMPULSE START

  function ImpulseResponse(url) {
    this.url = url;
    this.startedLoading = false;
    this.isLoaded = false;
    this.buffer = null;
  }

  ImpulseResponse.prototype.load = function() {
    if (this.startedLoading) {
      return;
    }

    var request = new XMLHttpRequest();
    request.open("GET", this.url, true);
    request.responseType = "arraybuffer";
    this.request = request;

    var asset = this;

    this.startedLoading = true;
    request.onload = function() {
      context.decodeAudioData(
        request.response,
        function(buffer) {
          asset.buffer = buffer;
          asset.isLoaded = true;
        },
        function(buffer) {
          console.log("Error decoding impulse response!");
        }
      );
    };
    request.send();
  };

  //IMPULSE END


  //SEQUENCER START

  //audio node variables
  var context;
  var convolver;
  var compressor;
  var masterGainNode;
  var effectLevelNode;
  var lowPassFilterNode;

  var noteTime;
  var startTime;
  var lastDrawTime = -1;
  var LOOP_LENGTH = 16;
  var rhythmIndex = 0;
  var timeoutId;
  var testBuffer = null;

  var currentKit = null;
  var reverbImpulseResponse = null;

  var tempo = 120;

  if (window.hasOwnProperty('AudioContext')) {
    window.audioContext = AudioContext;
  }

    init();
    toggleSelectedListener();
    playPauseListener();
    lowPassFilterListener();
    reverbListener();
    createLowPassFilterSliders();

  function createLowPassFilterSliders() {
    // $("#freq-slider").slider({
    //   value: 1,
    //   min: 0,
    //   max: 1,
    //   step: 0.01,
    //   disabled: true,
    //   slide: changeFrequency
    // });
    // $("#quality-slider").slider({
    //   value: 0,
    //   min: 0,
    //   max: 1,
    //   step: 0.01,
    //   disabled: true,
    //   slide: changeQuality
    // });
  }

  function lowPassFilterListener() {
    // $('#lpf').addEventListener('click',function() {
    //   $(this).toggleClass("active");
    //   $(this).blur();
    //   if ($(this).hasClass("btn-default")) {
    //     $(this).removeClass("btn-default");
    //     $(this).addClass("btn-warning");
    //     lowPassFilterNode.active = true;
    //     $("#freq-slider,#quality-slider").slider( "option", "disabled", false );
    //   }
    //   else {
    //     $(this).addClass("btn-default");
    //     $(this).removeClass("btn-warning");
    //     lowPassFilterNode.active = false;
    //     $("#freq-slider,#quality-slider").slider( "option", "disabled", true );
    //   }
    // })
  }

  function reverbListener() {
    // $("#reverb").addEventListener('click',function() {
    //   $(this).toggleClass("active");
    //   $(this).blur();
    //   if ($(this).hasClass("btn-default")) {
    //     $(this).removeClass("btn-default");
    //     $(this).addClass("btn-warning");
    //     convolver.active = true;
    //   }
    //   else {
    //     $(this).addClass("btn-default");
    //     $(this).removeClass("btn-warning");
    //     convolver.active = false;
    //   }
    // })
  }

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


      // var span = this.children("span");
      // if(span.hasClass('glyphicon-play')) {
      //   span.removeClass('glyphicon-play');
      //   span.addClass('glyphicon-pause');
      // }
      // else {
      //   span.addClass('glyphicon-play');
      //   span.removeClass('glyphicon-pause');
      //   handleStop();
      // }
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
    loadImpulseResponses();
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
    // for now, the master volume is static, but in the future there will be a slider
    masterGainNode = context.createGain();
    masterGainNode.gain.value = 1; // reduce overall volume to avoid clipping
    masterGainNode.connect(finalMixNode);

    //connect all sounds to masterGainNode to play them

    //don't need this for now, no wet dry mix for effects
    // // Create effect volume.
    // effectLevelNode = context.createGain();
    // effectLevelNode.gain.value = 1.0; // effect level slider controls this
    // effectLevelNode.connect(masterGainNode);

    // Create convolver for effect
    convolver = context.createConvolver();
    convolver.active = false;
    // convolver.connect(effectLevelNode);

    //Create Low Pass Filter
    lowPassFilterNode = context.createBiquadFilter();
    //this is for backwards compatibility, the type used to be an integer
    lowPassFilterNode.type = (typeof lowPassFilterNode.type === 'string') ? 'lowpass' : 0; // LOWPASS
    //default value is max cutoff, or passing all frequencies
    lowPassFilterNode.frequency.value = context.sampleRate / 2;
    lowPassFilterNode.connect(masterGainNode);
    lowPassFilterNode.active = false;
  }

  function loadKits() {
    //name must be same as path
    var kit = new Kit("TR808");
    kit.load();

    //TODO: figure out how to test if a kit is loaded
    currentKit = kit;
  }

  function loadImpulseResponses() {
    reverbImpulseResponse = new ImpulseResponse("samples/IMPULSE/matrix-reverb2.wav");
    reverbImpulseResponse.load();
  }


  //TODO delete this
  function loadTestBuffer() {
    var request = new XMLHttpRequest();
    var url = "http://www.freesound.org/data/previews/102/102130_1721044-lq.mp3";
    request.open("GET", url, true);
    request.responseType = "arraybuffer";

    request.onload = function() {
      context.decodeAudioData(
        request.response,
        function(buffer) {
          testBuffer = buffer;
        },
        function(buffer) {
          console.log("Error decoding drum samples!");
        }
      );
    };
    request.send();
  }

  function playNote(buffer, noteTime) {
    if(buffer>7) return;
    buffer = soundKit[buffer];
    var voice = context.createBufferSource();
    voice.buffer = buffer;

    var currentLastNode = masterGainNode;
    if (lowPassFilterNode.active) {
      lowPassFilterNode.connect(currentLastNode);
      currentLastNode = lowPassFilterNode;
    }
    if (convolver.active) {
      convolver.buffer = reverbImpulseResponse.buffer;
      convolver.connect(currentLastNode);
      currentLastNode = convolver;
    }
    voice.connect(currentLastNode);
    voice.start(noteTime);
  }

  let step = 0;
  function schedule() {
    var currentTime = context.currentTime;

    // The sequence starts at startTime, so normalize currentTime so that it's 0 at the start of the sequence.
    currentTime -= startTime;

    while (noteTime < currentTime + 0.200) {
        var contextPlayTime = noteTime + startTime;
          const state = store.getState();
          const grid  = state.grid;
          tempo = state.bpm;
          var synthNotes = [];
          grid.forEach((row, y) => {
            console.log('row', step);
            console.log('step', row[step]);
            console.log('y', y);
            if (y > 7) {
              synthNotes[y-8] = (row[step]) ? 1:0;
            }
            else if (row[step]) {
              playNote(y, contextPlayTime);
            }
            synth.playNotes(synthNotes);
          });
          step = (step + 1) % grid[0].length;

        // $currentPads.each(function() {
        //   if ($(this).hasClass("selected")) {
        //     var instrumentName = $(this).parents().data("instrument");
        //     switch (instrumentName) {
        //     case "kick":
        //       playNote(currentKit.kickBuffer, contextPlayTime);
        //       break;
        //     case "snare":
        //       playNote(currentKit.snareBuffer, contextPlayTime);
        //       break;
        //     case "hihat":
        //       playNote(currentKit.hihatBuffer, contextPlayTime);
        //       break;
        //   }
        //     //play the buffer
        //     //store a data element in the row that tells you what instrument
        //   }
        // });
        if (noteTime != lastDrawTime) {
            lastDrawTime = noteTime;
            drawPlayhead(rhythmIndex);
        }
        advanceNote();
    }

    timeoutId = requestAnimationFrame(schedule);
  }

  function drawPlayhead(xindex) {
      var lastIndex = (xindex + LOOP_LENGTH - 1) % LOOP_LENGTH;

      //can change this to class selector to select a column
      // var newRows = $('.column_' + xindex);
      // var oldRows = $('.column_' + lastIndex);
      //
      // $newRows.addClass("playing");
      // $oldRows.removeClass("playing");
  }

  function advanceNote() {
      // Advance time by a 16th note...
      // var secondsPerBeat = 60.0 / theBeat.tempo;

      var secondsPerBeat = 60.0 / tempo;
      rhythmIndex++;
      if (rhythmIndex == LOOP_LENGTH) {
          rhythmIndex = 0;
      }

      //0.25 because each square is a 16th note
      noteTime += 0.25 * secondsPerBeat;
  }

  function handlePlay(event) {
      console.log('START');
      rhythmIndex = 0;
      noteTime = 0.0;
      startTime = context.currentTime + 0.005;
      schedule();
      handlePlay.clicked = true;
  }

  function handleStop(event) {
    console.log('STOP');
    cancelAnimationFrame(timeoutId);
    var cells = document.getElementsByClassName("cell");
    for (var i = 0; i < cells.length; i++) {
        removeClass(cells[i],"playing");
    }
    handlePlay.clicked = false;
  }

  function removeClass(el, className){
    if (el.classList)
        el.classList.remove(className);
      else if (hasClass(el, className)) {
        var reg = new RegExp('(\\s|^)' + className + '(\\s|$)');
        el.className=el.className.replace(reg, ' ');
      }
  }
