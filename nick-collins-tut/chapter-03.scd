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
