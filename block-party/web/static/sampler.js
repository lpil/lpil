const sample_paths = [
  "/samples/tr-808/kick-2.wav",
  "/samples/tr-808/snare-2.wav",
  "/samples/tr-808/clap-2.wav",
  "/samples/tr-808/closed-hat-2.wav",
  "/samples/tr-808/open-hat-2.wav",
  "/samples/tr-808/cowbell-2.wav",
];

function play(n) {
  const audio = new Audio(sample_paths[n]);
  audio.play();
}

export { play };
