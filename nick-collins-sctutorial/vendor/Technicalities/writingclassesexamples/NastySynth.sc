NastySynth {
	classvar <>wackiness=200;

	*ar { arg wackymult=1; 
		^CombN.ar(
			SinOsc.ar(
				LFNoise0.ar(9, wackymult*wackiness, MouseY.kr(100,400)), 
				7, 
				MouseX.kr(0.0,0.75)
			) % 0.3,
			0.3,
			0.3, 
			5
		)
	}

}