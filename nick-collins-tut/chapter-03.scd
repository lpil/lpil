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

// We can plot things!
Env([1,0,1],[1,1]).plot;
