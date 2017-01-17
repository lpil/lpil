require("whatwg-fetch");

const ctx = new AudioContext();

const samplePaths = [
  "samples/tr808/kick.mp3",
  "samples/tr808/snare.mp3",
  "samples/tr808/clap.mp3",
  "samples/tr808/closed-hat.mp3",
  "samples/tr808/open-hat.mp3",
  "samples/tr808/cowbell.mp3",
].map(path =>
  `https://raw.githubusercontent.com/lpil/stepper/master/${path}`);


function sampleName(path) {
  const segments = path.split("/");
  const filename = segments[segments.length - 1];
  return filename.split(".")[0];
}


function loadSample(path) {
  return fetch(path)
    .then(res => res.arrayBuffer())
    .then(arr => ctx.decodeAudioData(arr))
    .then(buf => ({ name: sampleName(path), buffer: buf }));
}


function samplesToObject(samples) {
  console.log(samples);
  return samples.reduce((acc, s) => {
    acc[s.name] = s.buffer;
    return acc;
  }, {})
}


function buildSampler(buffers) {
  Object.freeze(buffers);

  function play(id) {
    const buffer = buffers[id];
    if (buffer) {
      const player = ctx.createBufferSource();
      player.buffer = buffer;
      player.connect(ctx.destination);
      player.start();
    }
  }

  return Object.freeze({ play });
}


function newSampler() {
  return Promise.all(samplePaths.map(loadSample))
    .then(samplesToObject)
    .then(buildSampler);
}


module.exports = newSampler;
