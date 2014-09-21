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
