"Hello mum";

s.stop;
s.boot;

(
{
	var sines = 5;
	var speed = 6;
	var f = { |x|
		var osc = SinOsc.ar(
			freq: (x + 1) * 100,
			mul: LFNoise1.kr(speed) + Line.kr(start: 1, end: -1, dur: 30),
		);
		Pan2.ar(in: osc, pos: rand2(1.0))
	};
	Mix.fill(sines, f) / sines
}.play;
)
