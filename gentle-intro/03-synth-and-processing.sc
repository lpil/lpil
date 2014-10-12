// UGens! Unit gnerators!

s.boot;

// Mouse control, instant Theremin
{ SinOsc.ar(freq: MouseX.kr(300, 2500), mul: MouseY.kr(0,1)) }.play;

// WE CAN PLOT THINGS!!!
{ SinOsc.ar }.plot;
{ Saw.ar }.plot;
{ Pulse.ar }.plot;

// WE CAN SCOPE THINGS
{ SinOsc.ar(freq: MouseX.kr(300, 2500), mul: MouseY.kr(0,1)) }.scope;

// .ar = audio rate
// .kr = control rate

// Control rate signals create fewer samples per unit time, and easier on the
// CPU as a result

// UGens basically just generate numbers
//    Unipolar UGens gen numbers between 0 and 1
//    Bipolar UGens gen numbers between -1 and 1

// We can use the .poll UGen method to get an indication of what numbers are
// being pumped out of it.
{ SinOsc.kr(1).poll }.play;
{ LFPulse.kr(1).poll }.play;

// Careful with volume here, it's an audio rate UGen being a bit horrible
{ LFNoise0.ar(1).poll }.play;
{ LFNoise0.ar(84).poll }.play;
{ LFNoise0.ar(2844).poll }.play;

{ MouseX.kr(minval: 300, maxval: 2500, lag: 10).poll }.play;

//
// Scaling ranges
//

// UGens can be the params of other UGens
{ SinOsc.ar(freq: LFNoise0.kr(10).range(500,1500), mul: 0.1) }.play;
// Which is...
{ LFNoise0.kr(1).poll }.play;
{ LFNoise0.kr(1).range(500,1500).poll }.play;
{ LFNoise0.kr(10).range(500,1500).poll }.play;
// See how .range is scaling the output of the UGen
// You could also do it with mul: and +
{ SinOsc.kr(1).range(100, 200).poll }.play;
{ SinOsc.kr(1, mul: 50, add: 150).poll }.play;

// Page 70
