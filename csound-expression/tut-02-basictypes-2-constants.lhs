github.com/anton-k/csound-expression/
  blob/master/tutorial/chapters/BasicTypesTutorial.md

> module BasicTypes where
> import Csound.Base


Constant Numbers (D)
--------------------
(and envelopes)


Linear Envelope generator
-- leg :: D -> D -> D -> D -> Sig

eXponential Envelope generator
-- xeg :: D -> D -> D -> D -> Sig

They take 4 args:
 attack time
 decay time
 sustain time
 release time

The new type, D, is the type for constant doubles

It's like Haskell's Double, but embedded in Csound

This is a signal that gradually changes pitch

> zero :: IO ()
> zero = dac $ setDur 5 $ mul 0.4 sound
>   where
>     sound = tri (50 + 150 * leg 2 2 0.5 1)

Notice the release time never comes into effect- this is because we never send
a note off.

We can use a virtual midi device to simulate a midi keyboard

> one :: IO ()
> one = vdac $ midi $ onMsg sound
>   where
>     sound x = mul 0.4 $ tri (x + 440 * leg 1 1 0.5 1)


We're going to be using D a lot.

Converting Double to D
-- double :: Double -> D

Converting D to Sig
-- sig :: D -> Sig


There are also more generic functions for making envelopes
-- linseq :: [D] -> Sig
-- expseq :: [D] -> Sig

They construct a linear or exponential envelope with any number of points
args:
 linseg [a, timeAB, b, timeBC, c, timeCD, d, ...etc]


They are alternating values and time stamps that progress from one pair
to another. Values for expseq should be greater than 0

There are also these two
-- linsegr :: [D] -> D -> D -> Sig
-- expsegr :: [D] -> D -> D -> Sig

They are the same, only they have a release. Good for midi instruments
  arg 2: release time
  arg 3: final release value

> two :: IO ()
> two = vdac $ midi $ onMsg sound
>   where
>     env     = expsegr [1,0.5, 0.1,0.3, 0.9,0.2, 0.4,0.2] 2 2
>     sound x = mul env $ tri (x + 440 * env)


Here's another two good ones. No points for guessing what they do.
-- fadeIn  :: D -> Sig
-- fadeOut :: D -> Sig

They operate from 0 to 1 (or 1 to 0)

This is like fadeIn followed by fadeOut
-- fades :: D -> D -> Sig
