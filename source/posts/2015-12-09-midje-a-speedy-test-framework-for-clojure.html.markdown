---
title: Midje, a speedy test framework for Clojure
date: 2015-12-09 18:14 UTC
tags:
  - Clojure
  - Testing
---

Anyone who has worked with me will know that I love tests. Can't get enough of
them. Center stage in my development feedback loop a good test framework and a
test runner, without either of these I feel much less productive, and
generally have less fun.

When I first started playing with Clojure I was impressed with the language,
It's functional, fast, has first class documentation, and feels incredibly
well designed. It even has a plethora of exciting libraries for making
[art][quil] and [music][overtone] that I could wait to get stuck into. It
seemed like the ideal language for me, yet somehow I didn't manage to get into
the flow of writing Clojure in the same way I had with Elixir, Ruby, and
Javascript. One part of the problem was the lack of a speedy test runner that
would automatically run my tests when I save a file. In Elixir land I wrote [a
little script][mix-test-watch] that would do this, but with the JVM taking
upwards of 6 seconds to start I'd need to build something a lot cleverer in
order to get the fast feedback I craved.

After listening to me moan my Clojure loving friend [jjl][jjl] recommended
[Midje][midje], a test framework that he claimed had completely changed his
approach to testing. After a few hours of playing about with it I was totally
sold.

It has a cute little set of test macros:

```clojure
(fact "all values except `false` and `nil` are truthy"
  (if true  :truthy :falsey) => :truthy
  (if []    :truthy :falsey) => :truthy
  (if ""    :truthy :falsey) => :truthy
  (if 0     :truthy :falsey) => :truthy
  (if nil   :truthy :falsey) => :falsey
  (if false :truthy :falsey) => :falsey)
```

And better still, it has a great autotest feature that uses some sort of
arcane Lisp voodoo to run the tests with the JVM instance powering your REPL,
meaning no waiting for a new instance to start. And boy- it is *fast*.
Suddenly the second long pause with my quick and dirty [Elixir test
runner][mix-test-watch] feels sluggish.

To use this feature in your Midje enabled project just start a REPL and then
stick these expressions in there:

```clojure
(use 'midje.repl)
(autotest)
```

Now save a file. Bam. How fast was that?

Happy testing. :)


[quil]: https://github.com/quil/quil
[overtone]: https://github.com/overtone/overtone
[mix-test-watch]: https://github.com/lpil/mix-test.watch
[jjl]: https://github.com/jjl
[midje]: https://github.com/marick/midje
