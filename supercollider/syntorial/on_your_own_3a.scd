// Pretty sure I'm leaking nodes here.
// Need to work out how to free them correctly.

TempoClock.default.tempo = 60/60;

(
SynthDef(\beep, { |out, preamp = 1, amp = 1|
	var adsr = EnvGen.kr(Env.adsr(0.1, 1.7, 0, 0), doneAction: Done.freeSelf) * 6;
	var pulse = Pulse.ar(freq: 60, mul: adsr);
	var llfo = SinOsc.kr(freq: 2 ** 3, add: 420, mul: 95);
	var lfo = SinOsc.kr(freq: llfo * 2, add: 120, mul: 70);
	var lpf = LPF.ar(in: pulse, freq: lfo) * 1.3;
    Out.ar(out, lpf ! 2);
}).add;
)

(
k = Pbind(\instrument, \beep, \delta, 1).play(quant: 1);
)

k.stop;
