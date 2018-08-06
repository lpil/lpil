s.boot;
s.quit;

// Note: Don't forget doneAction: 2!
(
  {
    WhiteNoise.ar(Line.kr([0.2, 0.2], 0, 2, doneAction: 2))
  }.play;
)

(
  {
    SinOsc.ar(
      freq: [440, 440],
      mul: Line.kr(1, 0, 2, doneAction: 2)
    )
  }.play;
)

// First bass

(
  SynthDef.new("first-bass", {
    var volEnv, osc, synth;

    volEnv = Env([0, 1, 0.33, 0], [0.014, 0.8, 0.5]);
    osc    = Saw.ar(freq: 440);
    synth  = EnvGen.kr(volEnv, doneAction: 2) * osc;

    Out.ar([0, 1], synth);
  }).send(s);
)

x = Synth.new("first-bass");
