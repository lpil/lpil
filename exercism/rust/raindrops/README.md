# Raindrops

Write a program that converts a number to a string, the contents of which depends on the number's prime factors.

- If the number contains 3 as a prime factor, output 'Pling'.
- If the number contains 5 as a prime factor, output 'Plang'.
- If the number contains 7 as a prime factor, output 'Plong'.
- If the number does not contain 3, 5, or 7 as a prime factor,
  just pass the number's digits straight through.

## Examples

- 28's prime-factorization is 2, 2, 7.
  - In raindrop-speak, this would be a simple "Plong".
- 1755 prime-factorization is 3, 3, 3, 5, 13.
  - In raindrop-speak, this would be a "PlingPlang".
- The prime factors of 34 are 2 and 17.
  - Raindrop-speak doesn't know what to make of that,
    so it just goes with the straightforward "34".

## Rust Installation

Refer to the [exercism help page][help-page] for Rust installation and learning
resources.

## Writing the Code

Execute the tests with:

```bash
$ cargo test
```

All but the first test have been ignored.  After you get the first test to
pass, remove the ignore flag (`#[ignore]`) from the next test and get the tests
to pass again.  The test file is located in the `tests` directory.   You can
also remove the ignore flag from all the tests to get them to run all at once
if you wish.

Make sure to read the [Crates and Modules](https://doc.rust-lang.org/stable/book/crates-and-modules.html) chapter if you
haven't already, it will help you with organizing your files.

## Feedback, Issues, Pull Requests

The [exercism/xrust](https://github.com/exercism/xrust) repository on GitHub is the home for all of the Rust exercises. If you have feedback about an exercise, or want to help implement new exercises, head over there and create an issue. Members of the [rust track team](https://github.com/orgs/exercism/teams/rust) are happy to help!

If you want to know more about Exercism, take a look at the [contribution guide](https://github.com/exercism/x-common/blob/master/CONTRIBUTING.md).

[help-page]: http://exercism.io/languages/rust
[crates-and-modules]: http://doc.rust-lang.org/stable/book/crates-and-modules.html

## Source

A variation on a famous interview question intended to weed out potential candidates. [http://jumpstartlab.com](http://jumpstartlab.com)
