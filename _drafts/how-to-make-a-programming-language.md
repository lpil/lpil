---
title: How to make a programming language
tags:
  - Languages
  - Interpreters
---

What actually happens in an interpreter when it is evaluating your code?
Presumably some very impressive voodoo understandable only to doctors of
computer science. The kind of people who are fluent in C++ and can blast
through the Euler Project in Haskell before their morning Weetabix.

Right? Wrong. It's actually relatively straightforward and an interpreter can
be built by mere mortals such as you and I. Let's explore some basic language
interpreter concepts in Elixir, a nice friendly high level language. No
pointer arithmetic or segfaults here!

The first thing to know is that all code syntax can be expressed as a tree.
Take the expression below, in which a function named `+` is called with the
numbers `1` and `2`.

This can be expressed as a tree with `+` as the root, and the numbers as
its children.

```elixir
1 + 2
```
```
call: +
 ├── number: 1
 └── number: 2
```

We can do the same for more complex expression.

```elixir
1 + 2 == 3
```
```
call: ==
 ├── call: +
 │    ├── number: 1
 |    └── number: 2
 └── number: 3
```

Here the call to the `==` function is the root and it has two children, a call
to `+` and the number 3. The call to `+` has two children, the number 1
and the number 2.

