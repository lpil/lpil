"Hello World".postln;

s.boot;
s.quit;
// This shows a silly little server control window
s.makeWindow;
// This starts and stops recording
s.record;
s.stopRecording;

{ SinOsc.ar }.play;

{ SinOsc.ar( LFNoise0.kr(10).range(200, 900), mul: 0.1) }.play;

(
{ RLPF.ar( 
    Dust.ar([12, 15]),
    LFNoise1.ar([1, 2]).range(100, 3000),
    0.02
  ) }.play;
)

// SC operator precedence goes left to right, regardless of operator
5 + 2 * 2; // => 14

(
  10.do( {"Print this line over and over...".postln} );
  10.do( {"Print this line over and over...".scramble.postln} );
)

{ Saw.ar(LFNoise0.kr([2,3]).range(100,2000),
        LFPulse.kr([4,5]) * 0.1
      ) }.play;

// A global variable
c = Pbind(\note, Pwhite(0,10), \dur, 0.1);
c.play;

// Global variables with names of multiple letters start with ~
~test = 1;

(
  // This is a local variable. It only exists within this block
  var foo = "bar";
)


// And now I stick this in here to see what it does to github's language
// detection. Currently it thinks that this is a scala file.
(
  x = {
    var lfn = LFNoise0.kr(1);
    var saw = Saw.ar(
                freq: 30,
                mul: LFPulse.kr(
                        freq: LFNoise1.kr(1).range(1,10),
                        width: 0.1));
    var bpf = BPF.ar(
                in: saw,
                freq: lfn.range(500,2500),
                rq: 0.01,
                mul: 20);
    
    Pan2.ar(in: bpf, pos: lfn);
  }.play;
)
x.free;
