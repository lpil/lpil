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
// You could also do it with mul: and add:
//  mul: multiply
//  add: add
{ SinOsc.kr(1).range(100, 200).poll }.play;
{ SinOsc.kr(1, mul: 50, add: 150).poll }.play;

// linlin: convert a linear range to a linear range
// linexp: linear to exponential
// explin: exponential to linear
// expexp: exponential to exponential
a = [1,2,3,4,5,6,7];
// Rescale 0-127, linear to linear
a.linlin(1, 7, 0, 127).round(1);
// Rescale 0-127, linear to exponential
a.linexp(1, 7, 0.1, 127).round(1); // Don't use 0 for exp lower bound

// Stopping individual synths
a = { Saw.ar(LFNoise2.kr(8).range(1000, 2000), mul: 0.2) }.play;
b = { Saw.ar(LFNoise2.kr(7).range(100, 1000), mul: 0.2) }.play;
c = { Saw.ar(LFNoise0.kr(15).range(2000, 3000), mul: 0.2) }.play;
a.free;
b.free;
c.free;

// You can change synth/function params while the synth is running with set
x = { |freq = 440, amp = 0.1| SinOsc.ar(freq, 0, amp) }.play;
x.set(\freq, 778);
x.set(\amp, 0.5);
x.set(\freq, 920, \amp, 0.2);
x.free;

// We can open metering.
// The first 8 ins and outs are reserved for the audio interface
// The rest (127 total) can be used by us for whatever we want
s.meter;

// Routing

// Out Ugen is for routing
//  arg 1: The target bus
//  arg 2: The signal to send out
{ Out.ar(0, SinOsc.ar(440, 0, 0.1)) }.play; // left channel
{ Out.ar(1, SinOsc.ar(440, 0, 0.1)) }.play; // right channel
{ Out.ar(2, SinOsc.ar(440, 0, 0.1)) }.play; // probably not connected

{ Out.ar([0,1], SinOsc.ar(440, 0, 0.1)) }.play; // stereo!

// In is also for routing!
// The arg is the bus to take from

// Set up the effect
//   A band pass filter listening on bus 55
f = {
  Out.ar(0, BPF.ar(in: In.ar(55), freq: MouseY.kr(500,7000), rq: 0.1))
}.play;

// Set up the audio source
n = { Out.ar(55, WhiteNoise.ar(0.5)) }.play;
n.free;

// You can't route the source before the effect.
// We'll learn more about this later in section 42, "order of execution"

// Multichannel expansion
// Check this out in the meter window
s.meter;
{ Out.ar(0, Saw.ar(freq: [440, 570], mul: 0.2)) }.play;
// Here we passed a list of two notes into the UGen synth, and we got stereo
// audio out! This is _Multichannel Expansion_
// It's magic.

// If you use an array as one of the args of a UGen, the entire patch gets
// duplicated, once for each item in the array.
// These duplicated UGens are sent to adjacent buses.

a = {
  Out.ar(0, SinOsc.ar(freq: [800, 880],
                      mul: LFPulse.ar([2,3]) * 0.2 ))
}.play;
a.free

// We can have ins span multiple buses too. See the second arg to In
r = {
  Out.ar(0, FreeVerb.ar(In.ar(55,2), mix: 0.5, room: 0.9, mul: 0.4))
}.play;

a = {
  Out.ar(55, SinOsc.ar(freq: [800, 880], mul: LFPulse.ar([2,3]) * 0.2 ))
}.play;
a.free;
r.free;

// So all this bus number stuff is really horrible. How on earth are you
// supposed to remember what you connected where? Bring on the Bus object
~reverbBus = Bus.audio(s, 2);
r = {
  Out.ar(
    0, 
    FreeVerb.ar(In.ar(~reverbBus,2), mix: 0.5, room: 0.9, mul: 0.4))
}.play;

a = {
  Out.ar(
    ~reverbBus,
    SinOsc.ar(freq: [800, 880], mul: LFPulse.ar([2,3]) * 0.2 ))
}.play;

// Panning

p = {
  Pan2.ar(
    in: PinkNoise.ar,
    pos: SinOsc.kr(2), // Num between -1 and +1
    level: 0.1
  )
}.play;
// We can use SinOsc here as it's a bipolar UGen,
// it outputs nums beteen -1 and +1
p.free;

(
  x = {
    var lfn = LFNoise0.kr(1);
    var saw = Saw.ar(
                freq: 30,
                mul: LFPulse.kr(
                        freq: LFNoise1.kr(1).range(1,10),
                        width: 0.1));
    var bpf = BPF.ar(
                in: saw,
                freq: lfn.range(500,2500),
                rq: 0.01,
                mul: 20);
    
    Pan2.ar(in: bpf, pos: lfn);
  }.play;
)
x.free;

// Page 81
// Playing audio files

// Load
~buf1 = Buffer.read(s, "/home/louis/data/new-music/downtempo/Rena Jones - Indra's Web/01 Rena Jones - The Awe and the Wonder.flac");

// Play
{ Pan2.ar(in: PlayBuf.ar(1, ~buf1)) }.play;

// Getting some info on the file
[~buf1.bufnum, ~buf1.numChannels, ~buf1.path, ~buf1.numFrames];

// Changing the playback speed with `rate`

// Double speed
{ Pan2.ar(
  in: PlayBuf.ar(numChannels: 1, bufnum: ~buf1, rate: 2, loop: 1)
) }.play;

// Half speed
{ Pan2.ar(
  in: PlayBuf.ar(1, ~buf1, 0.5, 1)
) }.play;

// Speeding up
{ Pan2.ar(
  in: PlayBuf.ar(1, ~buf1, Line.kr(0.2, 2, 10), 1)
) }.play;

// Backwards
{ Pan2.ar(
  in: PlayBuf.ar(1, ~buf1, -1, loop: 1)
) }.play;

// Mouse control
{ Pan2.ar(
  in: PlayBuf.ar(1, ~buf1, MouseX.kr(-3, 3), 1)
) }.play;

s.meter;

// Synth nodes
s.plotTree;

// Synths are represented in the server as nodes
// Run each of these in series, and check out the node tree

w = { SinOsc.ar(60.midicps, 0, 0.1) }.play;
x = { SinOsc.ar(64.midicps, 0, 0.1) }.play;
y = { SinOsc.ar(67.midicps, 0, 0.1) }.play;
z = { SinOsc.ar(71.midicps, 0, 0.1) }.play;
w.free;
x.free;
y.free;
z.free;

// One thing that is annoying is that synths continue to exist on the server
// even if they are have finished, and get just generating silence. They need
// to be freed.

// `doneAction: 2` solves this for us! It automatically frees the synth node
// after it is finished.

// Here it is applied to the Line, which is acting as an envelope
{
  WhiteNoise.ar(Line.kr(0.2, 0, 2, doneAction: 2))
}.play;

// Here it is applied to the playing audio buffer
{
  PlayBuf.ar(1, ~buf1, rate: 20, doneAction: 2)
}.play;


// SPEAKING OF ENVELOPES
