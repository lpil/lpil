s.boot;
s.quit;

// P is for pattern
Pbind(\degree, Pseries(0,1,30), \dur, 0.05).play;

// This plays the middle note C once a second.
// \degree refers to scale degrees.
// 0 is the first note (index on the scale)
// C major scale is assumed
Pbind(\degree, 0).play;

// We can adjust the duration with the \dur keyword
Pbind(
  \degree, 0,
  \dur, 1.5
).play;

// Pseq deals with sequences
Pbind(
  // arg 1, a list of notes
  // arg 2, number of repetitions
  \degree, Pseq([0,1,2,3,4,5,6,7],3),
  \dur, 0.15
).play;

Pbind(
  \degree, Pseq([0,1,2,3,4,5,6,7],3),
  \dur, Pseq([1,2,3,4,5,6,7,8] / 100,3),
  // Hey, we can use operators ^
).play;

Pbind(
  \degree, Pseq([0,1,2,3,4,5,6,7],3),
  \dur, Pseq([0.3,0.6,0.9] / 5, inf),
  // We can use inf to make it loop
).play;

// When specifying frequencies there are keywords other than \degree
// \note     chromatic notes
// \mininote midi note numbers
// \freq     hertz

// These all play the same frequency!
Pbind(\degree,    5).play;
Pbind(\note,      9).play;
Pbind(\midinote, 69).play;
Pbind(\freq,    440).play;

// More keywords! \amp and \legato
Pbind(
  \degree, Pseq([0,1,2,3,4,5,6,7],3),
  \dur, 0.25,
  \amp, Pseq([0,1,2,3,4,5,6,7] / 10,inf),
  \legato, 0.1
).play;


// Prand is like Pseq, but with a random order
Pbind(
  \degree, Prand([0,1,2,3,4,5,6,7],3 * 7),
  \dur, 0.25,
  \amp, Pseq([0,1,2,3,4,5,6,7] / 10,inf),
  \legato, 0.1
).play;

// Pwhite is another one.
// It gives you evenly distributed random numbers between 2 values
Pbind(
  \degree, Pwhite(0, 7 * 5) - (7*2),
  \dur, 0.25,
  \amp, Pseq([0,1,2,3,4,5,6,7] / 10, 16),
  \legato, 0.6
).play;

// If Pwhite's args are ints, you only get ints. To get floats, give one
Pwhite(0, 1.0);

// A Pbind stops when it's shortest internal pattern is done

// This play the length of array * 4 notes
Pbind(
  \degree, Pseq([0,2,3,5,7,8,11,12], 4),
  \dur, 0.3
).play;
// This plays 4 notes
Pbind(
  \degree, Prand([0,2,3,5,7,8,11,12], 4),
  \dur, 0.3
).play;

// New pattern things

// Pser
// Like Pseq, exept it takes N values from the array, rather than looping
// through it N times
Pbind(
  \note, Pser([0,2,3,5,7,8,11,12], 11),
  \dur, 0.15
).trace.play;

// Pxrand
// Like Prand, but doesn't repeat the same ele twice in a row
Pbind(
  \note, Pxrand([0,2,3,5,7,8,11,12], inf),
  \dur, 0.15
).trace.play;

// Pshuf
// Like Pseq, but it shuffles the array first
Pbind(
  \note, Pshuf([0,2,3,5,7,8,11,12], 2),
  \dur, 0.15
).trace.play;

// Pslide
// args: list, repeats, length, step
// Uh... I can't explain this one
Pbind(
  \note, Pslide([0,2,3,5,7,8,11,12], 5, 3, 0),
  \dur, 0.15
).trace.play;


// Pseries
// args: start, step, length
// Just a range function, basically
Pbind(
  \note, Pseries(0,2,15),
  \dur, 0.15
).trace.play;

// Pgeom
// args: start, grow, length
// Series that increases in size by grow arg each time
Pbind(
  \note, Pseq([0,1,2,3,4,5,6,7], inf),
  \dur, Pgeom(0.1, 1.1, 25)
).trace.play;

// Pn
// Repeats the arg1 arg2 times
Pbind(
  \note, Pn(0, 3),
  \dur, 0.5
).trace.play;

// You can nest these P things
Pbind(
  \note, Pseq([-2, Pn(0,3), 2, Pn(4,2)]),
  \dur, 0.2
).trace.play;

// Nest lists to get chords
Pbind(
  \note, Pseq([[0,3,7], [2,5,8], [3,7,10], [5,8,12]], 4),
  \dur, Pgeom(0.2, 1.15, inf)
).trace.play;

// Strum
// Doesn't seem to work?
Pbind(
  \note, Pseq([-7,3,7,10], [0,3,5,8], 2),
  \dur, 0.7,
  \legato, 0.4,
  \strum, 0.9
).trace.play

// Scales!
// These only work with \scale
Pbind(
  \scale, Scale.harmonicMinor,
  \degree, Pseq([0,1,2,3,4,5,6,7], 1),
  \dur, 0.15
).play;
// Evalute this to get a list of scales
Scale.directory;

// If you need a note that is outside of the scale you can do this
Pbind(
  \scale, Scale.harmonicMinor,
  \degree, Pseq([0,1,2,3,3.1,4], 1),
  //                     ^^^ .1 means 1 chromatic step up
  \dur, 0.15
).play;
// Without \scale Scale.major is assumed

// Transposition
// You can get chromatic transposition like this. Won't work with \freq
Pbind(
  \degree, Pseq([0,1,2,3,4,5,6,7], 1),
  \ctranspose, 12, // transpose up an octave (12 semitones)
  \dur, 0.15
).play;

// Microtones
// With \note and \midinote
Pbind( \note, Pseq([0,0.5,1,1.5,1.75,2], 1),).play;
Pbind( \midinote, Pseq([60,69,68.5,60.25,70], 1),).play;

// Tempo!
// The default bpm is 60
Pbind(
  \degree, Pseq([0,1,2,3,4,5,6,7], 2)
).play(TempoClock(120/60)); // 120 beats in 60 seconds, aka 120BPM

Pbind(
  \degree, Pseq([0,1,2,3,4,5,6,7]) // <=
  // Hey look, we skipped the second arg. It has a default value
).play;

// Rests
Pbind(
  \degree, Pwhite(0,10),
  \dur, Pseq([0.1, 0.1, 0.3, 0.6, Rest(0.3), 0.25], inf)
).play;
// They can be used anywhere in a Pbind, not just in \dur

s.record;
s.stopRecording;
// To play 2 Pbinds at once, enclose them in a block
(
  Pbind(
    \freq, Pn(Pseries(110, 111, 10), 2),
    \dur, 1/2,
    \legato, Pwhite(0.1, 1)
  ).play;
  Pbind(
    \freq, Pn(Pseries(220, 222, 10), 4),
    \dur, 1/4,
    \legato, Pwhite(0.1, 1)
  ).play;
  Pbind(
    \freq, Pn(Pseries(330, 333, 10), 6),
    \dur, 1/6,
    \legato, 0.1
  ).play;
)

// In order to play Pbinds in a time ordered fashion you can use {}.fork
(
  {
    "one".postln;
    2.wait;
    "two".postln;
    2.wait;
    "three".postln;
  }.fork;
)

(
  t = TempoClock(64/60);
  {
    Pbind(
      \note, Pseq([[4,11],[6,9]], 32),
      \dur, 1/6,
      \amp, Pseq([0.1,0.06], inf)
    ).play(t);

    2.wait;

    Pbind(
      \note, Pseq([[-25,-13,-1],[-20,-8,4], \rest], 3),
      \dur, Pseq([1,1,Rest(1)], inf),
      \amp, 0.2,
      \legato, Pseq([0.4,0.7,\rest], inf)
    ).play(t);

    2.75.wait;

    Pbind(
      \note, Pseq([23,21,25,23,21,20,18,16,20,21,23,21], inf),
      \dur, Pseq([0.25,0.75,0.25,1.75,0.125,0.125,0.80,0.20,0.125,0.125,1], 1),
      \amp, 0.2,
      \legato, 0.5
    ).play(t);
  }.fork(t);
)

// Using the same sequence of numbers a lot? Save it in a variable
c = [0,2,3,5,7,8,11,12]

Pbind(\note, Pseq(c, 1), \dur, 0.15).play;
Pbind(\note, Prand(c, 6), \dur, 0.15).play;
Pbind(\note, Pslide(c, 5,3,1), \dur, 0.15).play;

~scale = [0,1,2,3,4,5,6,7];
~durs = [0.4, 0.2, 0.2, 0.4, 0.8, 0.2, 0.2, 0.2];
(
  // Play major scale
  Pbind(
    \degree, Pseq(~scale),
    \dur,    Pseq(~durs)
  ).play;
  // Play the same scale one octave higher, in reverse
  Pbind(
    \degree, Pseq(~scale.reverse + 7),
    \dur,    Pseq(~durs)
  ).play;
)

//
// Starting and stopping Pbinds independently
//

p = Pbind(
    \midinote, Pseq([57,62,65,67,69], inf),
    \dur, 1/7
  );
p.play; // This results in an EventStreamPlayer
        // Think of it as a musician playing the score (the Pbind) you eval'd
// To change a playing sound we need to call methods on the EventStreamPlayer
~myPlayer = p.play;
~myPlayer.stop;
~myPlayer.resume;
~myPlayer.stop.reset;
~myPlayer.start;
~myPlayer.stop;

// Top melody is from Tchaikovsky's Album for the Youth
// A lower melody is added in couterpoint
// Gentle intro page 38 + 39
(
  // This variable is local to the scope of this block
  var myDurs = Pseq([Pn(1,5),3,Pn(1,5),3,Pn(1,6),1/2,1/2,1,1,3,1,3], inf);

  // These ones are not
  ~upperMelody = Pbind(
    \midinote, Pseq([69,74,76,77,79,81,Pseq([81,79,81,82,79,81], 2),82,81,79,
                     77,76,74,74], inf),
    \dur, myDurs
  );
  ~lowerMelody = Pbind(
    \midinote, Pseq([57,62,61,60,59,58,57,55,53,52,50,49,50,52,50,55,53,55,57,
                     58,61,62,62], inf),
    \dur, myDurs
  );
)
(
  ~player1 = ~upperMelody.play;
  ~player2 = ~lowerMelody.play;
)
~player1.stop.reset;
~player2.stop.reset;
