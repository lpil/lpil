(function() {
  'use strict';

  var globals = typeof window === 'undefined' ? global : window;
  if (typeof globals.require === 'function') return;

  var modules = {};
  var cache = {};
  var has = ({}).hasOwnProperty;

  var aliases = {};

  var endsWith = function(str, suffix) {
    return str.indexOf(suffix, str.length - suffix.length) !== -1;
  };

  var unalias = function(alias, loaderPath) {
    var start = 0;
    if (loaderPath) {
      if (loaderPath.indexOf('components/' === 0)) {
        start = 'components/'.length;
      }
      if (loaderPath.indexOf('/', start) > 0) {
        loaderPath = loaderPath.substring(start, loaderPath.indexOf('/', start));
      }
    }
    var result = aliases[alias + '/index.js'] || aliases[loaderPath + '/deps/' + alias + '/index.js'];
    if (result) {
      return 'components/' + result.substring(0, result.length - '.js'.length);
    }
    return alias;
  };

  var expand = (function() {
    var reg = /^\.\.?(\/|$)/;
    return function(root, name) {
      var results = [], parts, part;
      parts = (reg.test(name) ? root + '/' + name : name).split('/');
      for (var i = 0, length = parts.length; i < length; i++) {
        part = parts[i];
        if (part === '..') {
          results.pop();
        } else if (part !== '.' && part !== '') {
          results.push(part);
        }
      }
      return results.join('/');
    };
  })();
  var dirname = function(path) {
    return path.split('/').slice(0, -1).join('/');
  };

  var localRequire = function(path) {
    return function(name) {
      var absolute = expand(dirname(path), name);
      return globals.require(absolute, path);
    };
  };

  var initModule = function(name, definition) {
    var module = {id: name, exports: {}};
    cache[name] = module;
    definition(module.exports, localRequire(name), module);
    return module.exports;
  };

  var require = function(name, loaderPath) {
    var path = expand(name, '.');
    if (loaderPath == null) loaderPath = '/';
    path = unalias(name, loaderPath);

    if (has.call(cache, path)) return cache[path].exports;
    if (has.call(modules, path)) return initModule(path, modules[path]);

    var dirIndex = expand(path, './index');
    if (has.call(cache, dirIndex)) return cache[dirIndex].exports;
    if (has.call(modules, dirIndex)) return initModule(dirIndex, modules[dirIndex]);

    throw new Error('Cannot find module "' + name + '" from '+ '"' + loaderPath + '"');
  };

  require.alias = function(from, to) {
    aliases[to] = from;
  };

  require.register = require.define = function(bundle, fn) {
    if (typeof bundle === 'object') {
      for (var key in bundle) {
        if (has.call(bundle, key)) {
          modules[key] = bundle[key];
        }
      }
    } else {
      modules[bundle] = fn;
    }
  };

  require.list = function() {
    var result = [];
    for (var item in modules) {
      if (has.call(modules, item)) {
        result.push(item);
      }
    }
    return result;
  };

  require.brunch = true;
  globals.require = require;
})();
require.register("audio", function(exports, require, module) {
'use strict';

Object.defineProperty(exports, '__esModule', {
  value: true
});

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { 'default': obj }; }

var _voice = require('voice');

var _voice2 = _interopRequireDefault(_voice);

var _midi_message = require('midi_message');

var _midi_message2 = _interopRequireDefault(_midi_message);

var Ctx = window.AudioContext || window.webkitAudioContext;
var ctx = new Ctx();
var voices = {};

function stop(note) {
  var voice = voices[note];
  delete voices[note];
  voice.stop();
}

function play(note) {
  if (voices[note]) {
    return console.log('Already playing ' + note);
  }
  var hz = _midi_message2['default'].noteToHz(note);
  var voice = new _voice2['default'](ctx, ctx.destination);
  voice.play(hz);
  voices[note] = voice;
}

exports['default'] = { play: play, stop: stop };
module.exports = exports['default'];
});

require.register("controllers/device_controller", function(exports, require, module) {
'use strict';

Object.defineProperty(exports, '__esModule', {
  value: true
});

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { 'default': obj }; }

var _audio = require('audio');

var _audio2 = _interopRequireDefault(_audio);

var TYPE_ON = 144;
var TYPE_OFF = 128;

function onMessage(msg) {
  var data = msg.data;
  var type = data[0] & 0xf0;
  // const channel = data[0] & 0xf;

  if (type === TYPE_ON) {
    _audio2['default'].play(data[1]);
  } else if (type === TYPE_OFF) {
    _audio2['default'].stop(data[1]);
  }
}

exports['default'] = { onMessage: onMessage };
module.exports = exports['default'];
});

require.register("device", function(exports, require, module) {
'use strict';

Object.defineProperty(exports, '__esModule', {
  value: true
});

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { 'default': obj }; }

var _midi_access = require('./midi_access');

var _midi_access2 = _interopRequireDefault(_midi_access);

var device = {};

function registerRISE(controller, input, output) {
  if (input && output) {
    device.input = input;
    device.output = output;
    input.onmidimessage = controller.onMessage;
    console.log('Registered RISE');
  } else {
    throw 'ERROR: Launchpad MIDI device not found';
  }
}

function initDevice(controller) {
  _midi_access2['default'].getDevice(function (i, o) {
    return registerRISE(controller, i, o);
  }, /^Seaboard RISE$/);
}

exports['default'] = initDevice;
module.exports = exports['default'];
});

require.register("main", function(exports, require, module) {
'use strict';

Object.defineProperty(exports, '__esModule', {
  value: true
});

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { 'default': obj }; }

var _device = require('device');

var _device2 = _interopRequireDefault(_device);

var _controllersDevice_controller = require('controllers/device_controller');

var _controllersDevice_controller2 = _interopRequireDefault(_controllersDevice_controller);

(0, _device2['default'])(_controllersDevice_controller2['default']);

exports['default'] = function () {
  return console.log('Hello, world');
};

module.exports = exports['default'];
});

require.register("midi_access", function(exports, require, module) {
'use strict';

Object.defineProperty(exports, '__esModule', {
  value: true
});
function findDevice(devices, nameRegex) {
  var result = devices.next();
  while (!result.done) {
    if (nameRegex.test(result.value.name)) {
      return result.value;
    }
    result = devices.next();
  }
}

// Callback gets the input and output as arguments
// Name should be a regex
//
function getDevice(cb, nameRegex) {
  navigator.requestMIDIAccess().then(function (midiAccess) {
    var inputs = midiAccess.inputs.values();
    var outputs = midiAccess.outputs.values();
    var input = findDevice(inputs, nameRegex);
    var output = findDevice(outputs, nameRegex);
    cb(input, output);
  }, function () {
    throw new Error('Failed to get midi access.');
  });
}

exports['default'] = { getDevice: getDevice };
module.exports = exports['default'];
});

require.register("midi_message", function(exports, require, module) {
'use strict';

Object.defineProperty(exports, '__esModule', {
  value: true
});
function noteToHz(note) {
  return 440.0 * Math.pow(2, (note - 69.0) / 12.0);
}

function msgToHz(msg) {
  return noteToHz(msg.data[1]);
}

exports['default'] = { noteToHz: noteToHz, msgToHz: msgToHz };
module.exports = exports['default'];
});

require.register("reverb", function(exports, require, module) {
'use strict';

Object.defineProperty(exports, '__esModule', {
  value: true
});
function impulseResponse(ctx, duration, decay) {
  var sampleRate = ctx.sampleRate;
  var length = sampleRate * duration;
  var impulse = ctx.createBuffer(2, length, sampleRate);
  var impulseL = impulse.getChannelData(0);
  var impulseR = impulse.getChannelData(1);

  if (!decay) {
    decay = 2.0;
  }
  for (var i = 0; i < length; i++) {
    impulseL[i] = (Math.random() * 2 - 1) * Math.pow(1 - i / length, decay);
    impulseR[i] = (Math.random() * 2 - 1) * Math.pow(1 - i / length, decay);
  }
  return impulse;
}

function createReverb(ctx) {
  var convolver = ctx.createConvolver();
  convolver.buffer = impulseResponse(ctx, 0.5, 8, false);
  return convolver;
}

exports['default'] = createReverb;
module.exports = exports['default'];
});

require.register("voice", function(exports, require, module) {
'use strict';

Object.defineProperty(exports, '__esModule', {
  value: true
});

var _createClass = (function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ('value' in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; })();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError('Cannot call a class as a function'); } }

var attackTime = 0.05;
var releaseTime = 0.2;

var Voice = (function () {
  function Voice(ctx, out) {
    _classCallCheck(this, Voice);

    var oscNode = ctx.createOscillator();
    var gainNode = ctx.createGain();

    oscNode.type = 'triangle';
    oscNode.connect(gainNode);
    gainNode.connect(out);

    this.oscNode = oscNode;
    this.freqObj = oscNode.frequency;
    this.gainObj = gainNode.gain;
    this.ctx = ctx;
  }

  _createClass(Voice, [{
    key: 'play',
    value: function play(hz) {
      this.freqObj.value = hz;
      var now = this.ctx.currentTime;
      this.oscNode.start();
      this.gainObj.cancelScheduledValues(now);
      this.gainObj.setValueAtTime(0, now);
      this.gainObj.linearRampToValueAtTime(0.5, now + attackTime);
    }
  }, {
    key: 'stop',
    value: function stop() {
      var _this = this;

      var now = this.ctx.currentTime;
      this.gainObj.cancelScheduledValues(now);
      this.gainObj.setValueAtTime(this.gainObj.value, now);
      this.gainObj.linearRampToValueAtTime(0, now + releaseTime);
      setTimeout(function () {
        console.log('hi');_this.oscNode.stop();
      }, 1000 * releaseTime * 2);
    }
  }]);

  return Voice;
})();

exports['default'] = Voice;
module.exports = exports['default'];
});

;
//# sourceMappingURL=main.js.map