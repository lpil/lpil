s.boot;

{ [SinOsc.ar(440, 0, 0.2), SinOsc.ar(442, 0, 0.2)] }.play;

"Hello World!".postln;

(
  "Hello World!".postln;
  "Hello yourself!".postln;
)

{ SinOsc.ar([440, 442], 0, 0.2) }.play;

// Function definition:
f = { "Function evaluated"; };
f.(); // This is how we call a function.

f = { rand(3.0); };
f.();

// A function with an arg
f = { |x| rand(x); };
f.(3.0);
f.(3);

// A function with a local variable
f = { |x|
  var xx = x * x;
  xx;
};
f.(4);


//
// Single letter variables are "interpreter variables".
//  Effectively they are globals.
//

(
  var myFunc = { "Hello, world!"; };
  myFunc.();
)
myFunc.(); // ERROR

// kr Ugen as an arg for an ar Ugen
(
  { var ampOsc;
        ampOsc = SinOsc.kr(0.5, 1.5pi, 0.5, 0.5); // 1.5pi means 1.5 * PI
        SinOsc.ar(440, 0, ampOsc);
  }.play;
)
