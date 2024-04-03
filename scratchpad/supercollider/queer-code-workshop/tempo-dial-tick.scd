

MIDIIn.connectAll

(
SynthDef(\click, {|amp = 20|
	var beat;
	beat = FreeVerb.ar(Resonz.ar(RHPF.ar(WhiteNoise.ar(EnvGen.kr(Env([0, 0.005, 0], [0.005, 0.05, 0], \lin, releaseNode:2), doneAction:2)), 1000), 30000, 0.9, 100), 0.6, 0.01, 0.8, amp/3);
	Out.ar(0, [beat,beat]);
}).add;
)

t = TempoClock.default;
t.tempo = 140/60;

(
Pdef(\pattern, Pbind(
	\instrument, \click,
	\dur, Pseq([1], inf),
	\amp, 25
))
)

Pdef(\pattern).play(quant: 1)


(
~midiListener = MIDIFunc.cc({ |midi|
	midi.postln;
	~newValue = t.tempo + ((midi - 64) / 100);
	~newValue.postln;
	t.tempo = ~newValue.abs
});
)
