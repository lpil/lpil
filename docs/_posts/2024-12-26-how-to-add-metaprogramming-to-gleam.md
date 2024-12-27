---
title: How to add metaprogramming to Gleam
tags:
  - gleam
---

It's common for people to ask "will Gleam get metaprogramming?". It's a good
question! A question that has a relatively complex answer, one that takes time
to give. This post is an attempt to condense this conversation I've had many
times into one document so that folks can get their answer more easily.

## What is metaprogramming anyway?

The word "metaprogramming" is not like "parametric polymorphism" or "tail call
optimisation". It is not the name of a feature, it is the name of a family of
features. Two people could both use the word "metaprogramming" and be talking
about two entirely different features that behave very differently.

- A C++ programmer may think of [template classes and template functions](https://en.cppreference.com/w/cpp/language/templates).
- An Elixir programmer may think of [compile time code execution that manipulates code as data](https://hexdocs.pm/elixir/main/macros.html).
- A Java programmer may think of [runtime reflection](https://docs.oracle.com/javase/tutorial/reflect/index.html).
- A Ruby programmer may think of dynamic metaprogramming with [`#method_missing`](https://apidock.com/ruby/BasicObject/method_missing).
- A Zig programmer may think of [`comptime`](https://zig.guide/language-basics/comptime/).

And more!

To meaningfully discuss any addition everyone has to have a shared
understanding of precisely what the proposed addition is. Avoiding ambiguity
and jargon is vital, and there may be a more useful word than "metaprogramming".

## Evolving a programming language

Language design is hard. One can't evaluate a feature without taking into
account the context of the rest of the language and the ecosystem around it.
Every feature will interplay with every other feature in some way, and it could
be that a seemingly straightforward change has dire consequences elsewhere. For
example, if Gleam were to get an early-return feature with an explicit `return`
keyword then its much-loved [`use` feature](https://tour.gleam.run/advanced-features/use/)
would no longer work (as `use` depends on everything being an expression).

The impact on the users of the language needs to also be carefully considered.
How could a programmer misunderstand a feature? How could a programmer misuse
feature? What sort of a pickle could a programmer get themselves into with this
feature when having an especially unlucky or stressful day? We've all had days
where we look at the code we've written and regret the approach we've taken to
a problem. That sucks. We want that to happen as infrequently as possible.

The more context, the better. Figure out the wider consequences of the feature
are. See how it could change how Gleam is written, read, and learnt. Write a
bunch of (uncompilable) programs using this future-Gleam and see how it feels to
use. Update existing programs and libraries and see how it changes them.

Gleam aims to be predictable, reliable, easy to learn, and easy to read, so any
change to the language should not work against these goals.

## Putting the cart before the horse

Features are solutions to problems, and we cannot judge whether a solution is
good if we don't know what the problem being solved is.

Identity real problems and deficiencies with reading and writing Gleam today.
Not _predicted_ problems or _suspected_ problems. Actual concrete pains that are
present in real-world Gleam today. Persistent problems that cannot be
reasonably solved with any of the options we have today in Gleam.

List the current approaches folks take to solve these problems and put into
words why they are not enough. This will involve talking to other community
members to discover how they're approaching these problems, or even if they
consider them problem at all. It may be that during this process you discover a
satisfactory solution that you were not aware of!

And no, "I like this feature in this other language" and "it would be cool" are
not valid problems! 😁

## Compilers are just programs

If you want to add a feature to a program then you need to have an idea of how
it would be implemented, and in that respect compilers are no different from
any other program. Consider what the compiler is going to need to do when it
encounters code using the new feature. How it will analyse the code? What
information will it need to figure out? What code it will generate? Without
answers to these questions a feature cannot be implemented, no matter how
attractive and desirable it might be.

For example, if the feature is Lisp-style macros then the compiler is going to
need a way to evaluate Gleam code at compiler time. Currently the compiler
can't do this, so how would that be added? There's a great deal of complexity
there, especially since we want compilation to be fast!

## Make a proposal

Once you have done this research and design work then write it up into a
document. Detail all of the above, along with what you see as the pros and cons
of the design. Share it with the community for discussion, so they can evaluate
it and find anything that can be improved or any roadblocks that you hadn't
discovered. If the proposal is good then there will likely be a lot of
back-and-forth and iteration on the idea, so all the little details can be
figured out.

Be aware that the likelihood of any particular proposal being accepted is slim,
so try not to get too emotionally invested in any specific design. Even a
proposal that did not make it into the language is valuable as it will
influence and inform future work. This is especially true for the analysis of
problems with current-day Gleam; It is from out shortcomings in which we have
the most opportunity to learn and improve.

Thanks for reading 💖
