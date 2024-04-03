////////////////
//  synthdef  // 
////////////////

// {}.play allows us to get started quickly, but it is not the preferred way of
// making synthesizers in SuperCollider. Instead, we use SynthDef.

// The format is...

// SynthDef(\synthName, { /* UGen function */ }).add;

// Define it
(
  SynthDef(\sine, {
    var noise = SinOsc.ar( Rand(440, 880), 0, 0.2 );
    Out.ar( [0, 1], noise ); // connect to the first 2 outputs
  }).add;
)
// Start some instances
a = Synth(\sine);
b = Synth(\sine);
c = Synth(\sine);
d = Synth(\sine);
e = Synth(\sine);
// Free the instances
a.free;
b.free;
c.free;
d.free;
e.free;

// Flop allows you to apply a function to an array, a bit like map.
{ |e| e.free; }.flop.([a, b, c, d, e]);

a = { |e| postln(e); e + 1 };
b = { |e| postln(e); e * 2 };
c = { |e| postln(e); e * 10 };
d =  a + b + c;
d.value(1);
