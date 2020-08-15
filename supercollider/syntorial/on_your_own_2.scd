//
// Playing with LPFs
//

(
~synth = { |cutoff = 440|
	~source = SinOsc.ar(freq: 440);
	~filtered = LPF.ar(in: ~source, freq: cutoff);
	[~filtered, ~filtered]
}.play
)

~synth.set(\cutoff, 400)
~synth.set(\cutoff, 200)

~lfo = SinOsc.kr(freq: 1, mul: 100, add: 2000).scope
~synth.set(\cutoff, ~lfo)

(
~synth = { |lfo_rate = 3|
	~lfo = SinOsc.ar(freq: lfo_rate, add: 200, mul: 50);
	~source = SinOsc.ar(freq: 440);
	~filtered = LPF.ar(in: ~source, freq: ~lfo);
	[~filtered, ~filtered]
}.play
)

~synth.set(\lfo_rate, 8)
