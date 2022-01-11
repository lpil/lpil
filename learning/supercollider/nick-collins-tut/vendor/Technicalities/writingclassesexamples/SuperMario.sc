SuperMario {
	
	classvar hardhatcolour, >dungareesupplier, <>companyaffiliation; 	// all SuperMarios have these common properties
	var <>height, <moustache, jumpingpower, >graspofItalian;	// SuperMarios vary in these attributes

	
	*new { arg height=1.2; 
		^super.new.initSuperMario(height)
	} 	// super stands for the superclass, in this case Object

	initSuperMario { arg h=1.2;
		height = h;
		moustache = rrand(0.05, 0.1);	// from 5 to 10 centimetres moustache virility
		jumpingpower = rrand(0.3, 1.0);
		graspofItalian = rrand(0.5,1.0); 
	}

	// these are some silly synthesis functions 
	jump  {
		^SinOsc.ar(
			EnvGen.kr(Env([50, 2000, 50],[0.5,0.5]*jumpingpower), doneAction:2),
			0,
			Line.kr(0.5,0.0, jumpingpower)
		)
	}

	speak {
		^SinOsc.ar(
			LFNoise0.ar(16,100, EnvGen.kr(Env([500, 300],[graspofItalian]), doneAction:2)),			0,
			Line.kr(0.3,0.0, graspofItalian)
		)
	}

	impressladies {
		^RHPF.ar(
			BrownNoise.ar, 
			LFNoise0.kr(10*height,9000*moustache,1000)
		)
		* EnvGen.ar(Env([1.0,1.0,0.0],[0.4,0.3]), doneAction:2)
	}

	// a spwan to make the SuperMario character make noises
	dostuff {
		SystemClock.sched(0.0,{
			{Pan2.ar(this.decide,0)}.play;
			rrand(0.5,1.0)
		});
	}

	// this method makes the actual decision. 
	decide {
		^if(0.33.coin, { this.jump },
			{
				if(0.5.coin, { this.speak },{ this.impressladies });
			});
	}

}



UltraPlumber : SuperMario {
	var <>ubendification, <>craftiness, <>sizeoftool; 

	*new { arg height=1.2;
		^super.new(height).initUltraPlumber;
	}

	initUltraPlumber {
		ubendification= rrand(0.5,1.0);
		craftiness= rrand(0.5,1.0);
		sizeoftool= rrand(0.5,1.0);
	}

	plumb {
		^Saw.ar(
			Line.kr(100,20, ubendification, doneAction:2),
			Line.kr(0.3,0.0, ubendification)
		)
	}

	makeupbill {
		^HPF.ar(
			Pulse.ar(Pulse.ar(1000,0.3,200,300),0.5), LFNoise0.kr(10*craftiness,500,1000)
		)
		* EnvGen.ar(Env([0.3,0.3,0.0],[0.4,0.3]), doneAction:2)

	}

	impressladies {
		^RHPF.ar(
			BrownNoise.ar, LFNoise0.kr(10*height,9000*moustache*sizeoftool,1000)
		)
		* EnvGen.ar(Env([0.4,0.4,0.0],[0.4,0.3]), doneAction:2)
	}

	decide {
		// this replaces the decide method in the superclass SuperMario
		^this.perform([\plumb,\makeupbill,\impressladies].choose)	}
}





