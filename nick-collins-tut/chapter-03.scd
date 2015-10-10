s.boot;
s.scope;
s.makeGui;

// Envelopes are rad.

// Goes on forever
{ SinOsc.ar( 444, 0, 0.1 ) }.play;

// One second to silence
(
  {
    var env = Line.kr( 0.5, 0.0, 1.0, doneAction: 2 );
    SinOsc.ar( 444, [0, 0], env )
  }.play;
)
// `doneAction: 2` means that the synth will be terminated once the end of the
// envelope has been reached.

// We can plot envelopes!
(
  Env(
    [1,0,1],
    [1,1]
  ).plot;
)
// Env is for making arbitrary envelopes. It takes 2 arrays of numbers.
// The first is a series of Y positions.
// The second is the distances on the X axis the values are seperated by.

// 1s for 0 to 1, then 0.5s for 1 to 0
Env([0, 1, 0], [1.0, 0.5]).plot;


// Env.linen
// * attackTime
// * sustainTime
// * releaseTime
// * level
// * curve
Env.linen(0.03, 0.3, 0.6, 2, 3.3).plot;

// Env.adsr
// * attackTime
// * decayTime
// * sustainLevel
// * releaseTime
// * peakLevel
// * curve
Env.adsr(0.01, 0.5, 0.5, 0.1, 2.0, 2).plot;
// Sustain doesn't get plotted as it depends on the trigger input.

// Env.perc (good for percussive hit sounds)
// * attackTime
// * releaseTime
// * level
// * curve
Env.perc(0.05, 0.5, 1, 0).plot;

// And many more! Huzzar.



// Let's do some synthesis with envelopes

// Make an env. 1s from 1 to 0.
Env([1, 0], [1.0]).plot;

// We can use EnvGen to turn an Envelope into a signal
// It's far too slow for our ears to hear.
(
  {
    var env = Env([1, 0], [1.0]);
    EnvGen.ar(env)
  }.scope;
)

// Let's multiply a simple sine tone by the env
(
  {
    var env = Env([1, 0], [1.0]);
    SinOsc.ar(440, [0,0], 0.5) * EnvGen.ar(env)
  }.play;
)





// Getting more complex!

// Let's use an env for frequency.
(
  {
    var freqEnv = EnvGen.ar( Env([1000, 10], [0.5]) );
    Saw.ar([freqEnv, freqEnv]);
  }.play;
)

// And another for amplitude
(
  {
    var freqEnv = Env([1000, 10], [0.5]);
    var ampEnv  = Env([0.5, 0],   [2.0]);

    freqEnv = EnvGen.ar( freqEnv, doneAction: 2 );
    ampEnv  = EnvGen.kr( ampEnv,  doneAction: 2 ); // Note this is kr

    Saw.ar([freqEnv, freqEnv], ampEnv);
  }.play;
)

// Oooooh, FM synthesis
(
  {
    var freq = SinOsc.ar(3000, 0, 10, 220);
    var amp = EnvGen.kr(Env([0.5,0.0], [1.0]), doneAction:2);
    SinOsc.ar( freq, [0,0], amp );
  }.play
)

(
  {
    var freq = EnvGen.kr(Env([500,100],[1.0]),doneAction:2);
    Saw.ar(freq, [0.5, 0.5]);
  }.play;
)


Line		// Straight line generator
XLine		// Exponential line generator

// Linear
(
  {
    var freq  = SinOsc.ar(1,0,10,440);
    var amp   = Line.kr(0.0001,1,1,doneAction:2);
    var noise = Saw.ar( freq, amp );
    Pan2.ar( noise, 0 );
  }.play;
)
// Exponential
(
  {
    var freq  = SinOsc.ar(1,0,10,440);
    var amp   = XLine.kr(0.0001,1,1,doneAction:2);
    var noise = Saw.ar( freq, amp );
    Pan2.ar( noise, 0 );
  }.play;
)

/////////////////////////////
//   Releasing envelopes   //
/////////////////////////////

// Some envelopes run through their stages without intervention.
(
  {
    var env = Env([0, 0.1, 0], [0.1, 0.9]);
    var amp = EnvGen.ar( env, doneAction: 2 );
    amp * SinOsc.ar(330);
  }.play
)
// Others will freeze at a certain point, waiting for instructions to begin the
// release phase.
(
  a = {
    var env = Env.asr( 0.1, 0.1, 0.9 );
    var amp = EnvGen.ar( env, doneAction: 2 );
    amp * SinOsc.ar(330);
  }.play
)
a.release(2.0); // Release, taking 2 seconds.

// Similar, but the synth function takes a gate argument, which holds the
// envelope at the releaseNode until its value is 1
(
  a = { |gate = 1|
    var amp = EnvGen.ar(Env.asr(0.1,0.1,0.9), gate, doneAction:2);
    amp * SinOsc.ar(330);
  }.play
)
// When gate is set to 0, the envelope can finish, and takes 0.9 seconds to
// fade out (releaseTime argument to Env.asr set above)
a.set(\gate, 0)


// With envelopes of arbitrary length we have to specify which point is the
// releaseNode ourselves, using a number.
//
// The number is the index - 1 of the value in the first array, and the index
// in the second array.
(
  // releaseNode is 1
  //  first array value:  0.0
  //  second array value: 3.0
  a = { |gate = 1|
    var env = Env([0.2, 1.0, 0.0], [0.1, 3.0], 0, releaseNode: 1);
    var amp = EnvGen.ar( env, gate, doneAction: 2 );
    amp * Pan2.ar( SinOsc.ar(550, 0, 0.2), 0 );
  }.play;
)
a.set(\gate, 0);


///////////////////////////
//   Looping envelopes   // 
///////////////////////////

// There is also a loopNode. The env will loop between it and the node before
// the releaseNode.
(
  e = { |gate = 1|
    var env = Env(
      [0.0, 0.0, 1.0, 0.0],
      [0.5, 1.0, 2.0],
      0,
      releaseNode: 2,
      loopNode: 0,
    );
    var amp = EnvGen.ar( env, gate, doneAction: 2 );
    Pan2.ar( SinOsc.ar(550, 0, 0.3) * amp, 0 );
  }.play;
)
// 2 seconds to fade out, as value 2 in the second array is 2.0
e.set(\gate, 0);


// If the envelope is looping fast enough, you can get interesting control
// signals, and even head towards audio rate waveforms.
(
  e = Env(
    [0.0, 1.0, -1.9, 0.0],
    [0.01, 0.01, 2.0],
    0,
    releaseNode: 2, loopNode: 0
  );
)
e.plot;
(
  a = { |gate = 1|
    var mouseX = MouseX.kr(0.3, 2.0);
    var noise = EnvGen.ar( e, gate, timeScale: mouseX, doneAction: 2);
    Pan2.ar( noise, 0 );
  }.play;
)
a.set(\gate, 0); // Stops immediately since 2 seconds is slow for ar.
