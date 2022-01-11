s.boot;

// There's some weird flexible syntax here. These are exactly the same
5.dup(10);  // receiver notation
dup(5, 10); // functional notation

100.0.rand.round(0.01).dup(4)
dup(round(rand(100.0),0.01), 4)

// This is really ugly
(
  {
    CombN.ar(
      SinOsc.ar(
        midicps(
          LFNoise1.ar(3, 24, LFSaw.ar([5, 5.123], 0, 3, 80))
        ),
        0, 0.4
      ),
      1, 0.3, 2)
  }.play;
)

// Double quotes are strings
"Hello, mum";
// Single quotes and \prefixes are symbols.
'Foobar';
\Foobar;

// {}s delimit a function
  1.exprand(1000.0);    // => returns a random number
{ 1.exprand(1000.0); }  // => returns a function

// If else
// Hey, weird, it uses a function call syntax
if([true, false].choose, "true", "false").postln;

// `and` and `or` are methods
true.and(false);
true.or(false);

(
  var num = 1;
  var result = case
  { num == 0 } { "zero" }
  { num == 1 } { "one" }
  { num == 2 } { "two" }
  { false } { "something else" };
  result.postln;
)

// Function call syntax sucks.
f = { |a, b| a + b };
f.value(1,2);

//
// Arrays
//

a = [0,1,2,3,4,5];
a.reverse;
a.scramble;
a.choose; // Random ele
a.size;
a[0];
a[110]; // Out of bounds - Returns nil
a.wrapAt(10); // Like [], but wraps to start if index > size
[-2, -1] ++ a; // concat
a ++ "hi!" // a string is an array of chars
a ++ 'hi!' // a symbol is not
a.permute(3); // swap ele at index 0 with ele at index arg
a.mirror; // make palindrome
a.powerset; // combinations

// These have side effects
a.add(6); // push
a.insert(2, 999);
a.put(2, 8); // destructive
a;

// So map can be sort of implicit. Bit weird
a = [0,1,2,3,4,5];
a + 10;
a * 10;
(a / 3).round(0.1);
(
  var x = 11;
  var y = 12;
  [1, x, y] * 3;
)
[1, 2, 3, "hi", 5] + 10; // oops

// Creating arrays
// Arithmetic series
Array.series(size: 6, start: 10, step: 3);
// Geometric series
Array.geom(size: 10, start: 1, grow: 2);
// Fill
Array.fill(10, rrand(1, 10));
Array.fill(10, { rrand(1, 10) });
// Empty
Array.newClear(7);
// x ! y is sugar for x.dup(y);
"foo" ! 4;
"foo".dup(4);
// x..y is sugar for series. Makin ranges
(
  (50..59);
  Array.series(10, 50, 1);
)
(
  (50, 52..68);
  Array.series(10, 50, 2);
)

// Often we will need to do some action over all items of a collection.
// There is a method for this
~myFreqs = Array.fill(10, { rrand(440, 880) });
~myFreqs.do({ |e, i| ("Element " ++ i ++ " is " ++ e ++ " Hz." ).postln; });
~myFreqs.squared; // If there is a method on them you can do this
