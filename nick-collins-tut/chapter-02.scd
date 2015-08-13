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
