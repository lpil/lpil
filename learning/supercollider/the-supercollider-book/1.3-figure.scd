(
{
	var saw = LFSaw.ar(freq: [15, 15.123], iphase: 0, mul: 3, add: 40);
	var midiNumber = LFNoise1.ar(freq: 3, mul: 24, add: saw).midicps;
	var sine = SinOsc.ar(freq: midiNumber, phase: 0, mul: 0.4);
	CombN.ar(sine, maxdelaytime: 3, delaytime: 0.3, decaytime: 1.3)
}.play;
)
