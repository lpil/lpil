TempoClock.default.tempo = 90/60;

(
SynthDef(\beep, { |out, preamp = 1, amp = 1|
	var adsr = EnvGen.kr(Env.adsr(0.4, 0.7, 0, 0), doneAction: Done.freeSelf);
	var noise = SinOsc.ar(freq: 440, mul: adsr);
    Out.ar(out, noise ! 2);
}).add;
)

(
k = Pbind(\instrument, \beep, \delta, 1).play(quant: 1);
)

k.stop;
