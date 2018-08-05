github.com/anton-k/csound-expression/
  blob/master/tutorial/chapters/BasicTypesTutorial.md

> module BasicTypes where
> import Csound.Base


Signals (Sig)
------------

The Signal type is the most frequently used type for sounds.

> x = 1 :: Sig
> y = 2 :: Sig

We can think Sigs as a list of numbers

Ooh look, we can do math

> z = (x + y) * 0.5

We have Sigs that are simple waveforms
-- osc, saw, tri, sqr :: Sig -> Sig

The arg Sig is the frequency

> zero = dac $ tri 220

We can set a duration of a signal
 setDur should only be used once, right before the dac

> one = dac $ setDur 2
>                    $ tri 220

We can modulate the frequency with a low frequency oscillator

> twoSin = osc (220 + 100 * osc 0.5)
> twoTri = tri (220 + 100 * osc 0.5)
> twoSqr = sqr (220 + 100 * osc 0.5)
> twoSaw = saw (220 + 100 * osc 0.5)

These have all been pretty loud. We can adjust the volume using mul
three = dac $ mul 0.5 twoTri

We could have used `*`, but mul is more useful as it works for tuples and
side effecting signals too
