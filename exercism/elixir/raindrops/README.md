# Raindrops

Write a program that converts a number to a string, the contents of which depends on the number's factors.

- If the number contains 3 as a factor, output 'Pling'.
- If the number contains 5 as a factor, output 'Plang'.
- If the number contains 7 as a factor, output 'Plong'.
- If the number does not contain 3, 5, or 7 as a factor,
  just pass the number's digits straight through.

## Examples

- 28's factors are 2, 4, **7**, 14.
  - In raindrop-speak, this would be a simple "Plong".
- 30's factors are 2, **3**, **5**, 6, 15.
  - In raindrop-speak, this would be a "PlingPlang".
- 34 only has two factors- 2 and 17.
  - Raindrop-speak doesn't know what to make of that,
    so it just goes with the straightforward "34".

## Running tests

Execute the tests with:

```bash
$ elixir bob_test.exs
```

(Replace `bob_test.exs` with the name of the test file.)


### Pending tests

In the test suites, all but the first test have been skipped.

Once you get a test passing, you can unskip the next one by
commenting out the relevant `@tag :pending` with a `#` symbol.

For example:

```elixir
# @tag :pending
test "shouting" do
  assert Bob.hey("WATCH OUT!") == "Whoa, chill out!"
end
```

Or, you can enable all the tests by commenting out the
`ExUnit.configure` line in the test suite.

```elixir
# ExUnit.configure exclude: :pending, trace: true
```

For more detailed information about the Elixir track, please
see the [help page](http://exercism.io/languages/elixir).

## Source

A variation on a famous interview question intended to weed out potential candidates. [http://jumpstartlab.com](http://jumpstartlab.com)

## Submitting Incomplete Problems
It's possible to submit an incomplete solution so you can see how others have completed the exercise.

