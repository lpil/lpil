s.boot;

2 + 2;

"I am SuperCollider\n".post;

{ Pan2.ar(SinOsc.ar(440, 0, 0.5), 0.0) }.play;

(
  {
    var mouse = MouseX.kr(440,880),
        osc   = SinOsc.ar(mouse, 0, 0.5);
    Pan2.ar(osc, 0.0)
  }.play
)

(
  if (1 == 2) {
    "hi"
  } {
    "bye"
  }
)


// This is mental. I've no idea how it works.
( 
  { 
    var n = 11;
    // var n = 34;
    // var n = 3;
    var mouseX = MouseX.kr(100,2000);
    var mouseY = MouseY.kr(0.01,1.0);
    var noise = Mix.fill(n) { 
      var freq   = rrand(50,560.3);
      var numcps = rrand(2,20);
      var gendy  = Gendy1.ar(
        6.rand,
        6.rand,
        1.0.rand,
        1.0.rand,
        freq,
        freq,
        1.0.rand,
        1.0.rand,
        numcps,
        SinOsc.kr(exprand(0.02,0.2), 0, numcps/2, numcps/2), 0.5/(n.sqrt)
      );
      Pan2.ar(gendy, 1.0.rand2) 
    };
    Resonz.ar(noise, mouseX, mouseY); 
  }.play 
)

// Routing a SinOsc UGen into a Pan2
//
// frequency 440 -> SinOsc -> Pan2 -> output
//
{ Pan2.ar(SinOsc.ar(440,0,0.1), 0.0) }.play


(
  {
    var noise = SinOsc.ar(440,0,0.1) + Pulse.ar(443,0.6,0.05);
    Pan2.ar(noise, 0);
  }.play	
)

// Remember, you can look stuff up in the docs from your editor. Probably.
// If you're using Louis' computer and vim, try K.
LFSaw

{ RLPF.ar(Pulse.ar([100,250],0.5,0.1), XLine.kr(8000,400,5), 0.05) }.play;


// .postln and Post are useful for debuggin.
2.postln;
Post << [2,3,4,5] << nl; 
Post << [2,3,4,5];
