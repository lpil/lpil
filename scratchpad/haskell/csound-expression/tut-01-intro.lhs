github.com/anton-k/csound-expression/blob/master/tutorial/chapters/Intro.md

> module Intro where
> import Csound.Base

Lets play a sine wave at 440

> zero :: IO ()
> zero = dac $ osc 440

Magic!
