# Hello World

Write a function that greets the user by name, or by saying "Hello, World!" if no name is given.

["Hello, World!"](http://en.wikipedia.org/wiki/%22Hello,_world!%22_program) is
the traditional first program for beginning programming in a new language.

**Note:** You can skip this exercise by running:

    exercism skip $TRACK_ID hello-world

## Specification

Write a `Hello World!` function that can greet someone given their name.  The
function should return the appropriate greeting.

For an input of "Alice", the response should be "Hello, Alice!".

If a name is not given, the response should be "Hello, World!"

## Test-Driven Development

As programmers mature, they eventually want to test their code.

Here at Exercism we simulate [Test-Driven
Development](http://en.wikipedia.org/wiki/Test-driven_development) (TDD), where
you write your tests before writing any functionality. The simulation comes in
the form of a pre-written test suite, which will signal that you have solved
the problem.

It will also provide you with a safety net to explore other solutions without
breaking the functionality.

### A typical TDD workflow on Exercism:

1. Run the test file and pick one test that's failing.
2. Write some code to fix the test you picked.
3. Re-run the tests to confirm the test is now passing.
4. Repeat from step 1.
5. Submit your solution (`exercism submit /path/to/file`)

## Instructions

Submissions are encouraged to be general, within reason. Having said that, it's
also important not to over-engineer a solution.

It's important to remember that the goal is to make code as expressive and
readable as we can. However, solutions to the hello-world exercise will not be
reviewed by a person, but by rikki- the robot, who will offer an encouraging
word.


## Getting Started
For installation and learning resources, refer to the
[exercism help page](http://exercism.io/languages/ocaml).

## Installation
To work on the exercises, you will need `Opam` and `Core`. Consult [opam](https://opam.ocaml.org) website for instructions on how to install for your OS. Once `opam` is installed open a terminal window and run the following command to install core:

opam install core

## Running Tests
Because OCaml is a compiled language you need to compile your submission and the test code before you can run the tests. Compile with

```bash
$ corebuild -quiet test.native
```

and when successful run the tests by running the `test.native` executable:

```bash
./test.native
```

Alternatively just type

```bash
make
```

## Interactive Shell
`utop` is a command line program which allows you to run Ocaml code interactively. The easiest way to install it is via opam:
```bash
opam install utop
```
Consult [utop](https://github.com/diml/utop/blob/master/README.md) for more detail.

## Feedback, Issues, Pull Requests
The [exercism/xocaml](https://github.com/exercism/xocaml) repository on
GitHub is the home for all of the Ocaml exercises.

If you have feedback about an exercise, or want to help implementing a new
one, head over there and create an issue.  We'll do our best to help you!

## Use of `option`
The type signature of the `greet` function reads `string option -> string`. It
could be that this is your first time to encounter an Option type.

An option type is an explicit way to signal that a value could be missing for a
legitimate reason.

In OCaml Options types are implemented as
a [variant type](https://realworldocaml.org/v1/en/html/variants.html) something
like

```ocaml
type 'a option =
    | None
    | Some of 'a
```

In the case of `string option` you provide an argument using one of the
following snippets

1. `None` to signal that no subject is passed.
2. `Some("Alice")` to provide an actual subject.

Just like other variants types you can match on an option. The example below
demonstrates how this can be done. Here `x` is an `string option`.

```ocaml
match x with 
    | None           -> "no argument passed"
    | Some(argument) -> "argument passed"
```

## Source

This is an exercise to introduce users to using Exercism [http://en.wikipedia.org/wiki/%22Hello,_world!%22_program](http://en.wikipedia.org/wiki/%22Hello,_world!%22_program)

## Submitting Incomplete Problems
It's possible to submit an incomplete solution so you can see how others have completed the exercise.

