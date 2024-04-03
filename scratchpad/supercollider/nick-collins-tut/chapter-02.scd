s.boot;

"Chapter Two...".postln;

// Create a new frequency analyzer which will run in the BG
FreqScope.new;

{ Pan2.ar(SinOsc.ar(440, 0, 0.5), 0.0) }.play;


//
// Subtractive Synthesis
//

// Take a complex noise, and filter it until it's nice.

{ WhiteNoise.ar(0.5) }.scope; // Prepare yourself.

{ LPF.ar(WhiteNoise.ar(0.5), 1000) }.play; // And LP filtered

// We can create an envelope with Line

(
  {
    var osc = WhiteNoise.ar(0.5);
    var env = Line.kr(10000, 250, 5); // 10000 to 250 over 5 seconds
    LPF.ar(osc, env);
  }.play;
)

// Some oscillators
Saw
Blip

// Some noise sources
PinkNoise
LFNoise0

// Some filters
HPF
BPF
Resonz

(
  {
    var freq_env = Line.kr(880, 55, 7);
    Blip.ar(freq_env, 200, 0.5);
  }.play;
)


// Plug a noise source into a filter
(
  {
    var noise = LFNoise0.ar(400);
    Resonz.ar(noise,1000,0.1);
  }.scope;
)
// Modulate the filter's center frequency over time
(
  {
    var noise = LFNoise0.ar(400);
    var env   = Line.kr(10000, 1000, 10);
    Resonz.ar(noise, env, 0.1);
  }.play;
)


// Rather than starting with something complex and taking energy away to sculpt
// a sound, we can start with simple building blocks and add many of them
// together to create more involved sounds

// A sine wave. defaults to a concert A (440Hz)
{ SinOsc.ar }.scope

// We can sum them.
(
  {
    var osc1 = SinOsc.ar(400, 0, 0.1);
    var osc2 = SinOsc.ar(660, 0, 0.1);
    osc1 + osc2;
  }.scope;
)
// We can get *nearly* the same thing with array expansion.
{ SinOsc.ar([400, 660], 0, 0.1) }.scope;
{ SinOsc.ar([200, 330], 0, 0.1) }.scope;
{ SinOsc.ar([100, 165], 0, 0.1) }.scope;
{ SinOsc.ar([50,  82.5], 0, 0.1) }.scope;

// Interlude: PANNING!
//
// Pan2.ar( input signal, pan position )
(
  {
    var noise = BrownNoise.ar(0.1);
    var mouse = MouseX.kr(-1,1); // pan position is -1 to 1
    Pan2.ar(noise, mouse);
  }.scope
)

// When we used array expansion we split the audio over channels 0 and 1.
// If we use an array with more than 2 elements in it we'll only hear the first
// two UGens because only the first 2 channels are hooked up to our speakers.
// To get around this we mix them back together, with Mix
(
  {
    var freqs = [400, 660, 200, 330, 100, 165, 50, 82.5];
    var mix   = Mix(SinOsc.ar(freqs, 0, 0.1));
    Pan2.ar(mix, 0); // Play in stereo
  }.scope;
)

// In additive synthesis, if we know a recipe for the spectrum (frequency
// content) of a sound, we can synthesise it by adding up sine tones for each
// component frequency.
// Recipes for common waveforms are known from the Fourier theory of sound
// (sinusoids at which frequencies and amplitudes to add up to create certain
// waveform shapes).

// Sawtooth wave: Add up n harmonics with amplitude falling off as
// 1/harmonicnumber, sign alternates between +1 and -1
(
  {
    var num = 10;
    var fn  = { |i|
      var mult = 0.5 / (i+1);
      SinOsc.ar(440 * (i+1)) * mult;
    };
    var wave = Mix.fill(num, fn);
    Pan2.ar(wave / num, 0.0); // stereo, panned centre
  }.scope;
)

// Square wave: Sum of odd harmonics, no even, amplitude falls as off
// 1/harmonicnumber; closest 'real' waveform is a clarinet tone
(
  {
    var num = 10;
    var fn  = { |i|
      var harmonicnumber = 2 * i + 1; // odd harmonics only
      SinOsc.ar(440 * harmonicnumber) / harmonicnumber;
    };
    var wave = Mix.fill(num, fn) * 0.25;
    Pan2.ar(wave, 0); //stereo, panned centre
  }.scope;
)


// Bell sound example:

(
  {
    var root = 500;
    // This is a spectral recipe for a minor third bell
    var harmonics = root * [0.5, 1, 1.19, 1.56, 2, 2.51, 2.66, 3.01, 4.1];
    var mix = Mix(SinOsc.ar(harmonics, 0, 0.1));
    Pan2.ar(mix, 0);
  }.scope;
)

// We can even use different volumes

(
  {
    var root      = 500;
    var harmonics = root * [0.5, 1, 1.19, 1.56, 2, 2.51, 2.66, 3.01, 4.1];
    var volumes   = [0.25,1,0.8,0.5,0.9,0.4,0.3,0.6,0.1];
    var mix       = Mix(SinOsc.ar(harmonics, 0, volumes)) * 0.3;
    Pan2.ar(mix, 0);
  }.scope;
)

(
  var n = 9;
  var freqs = 125 * (1..n);
  {
    var noise = SinOsc.ar(freqs, 0, 1 / n);
    Mix(noise);
  }.play;
)
