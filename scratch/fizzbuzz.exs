defmodule FizzBuzz do
  defp fizzbuzzify(x) when rem(x, 15) == 0 do "FizzBuzz" end
  defp fizzbuzzify(x) when rem(x, 5)  == 0 do "Fizz" end
  defp fizzbuzzify(x) when rem(x, 3)  == 0 do "Buzz" end
  defp fizzbuzzify(x) do to_string x end

  def to(num) do
    stream = IO.stream :stdio, :line
    for x <- 1..num, into: stream do
      fizzbuzzify(x) <> "\n"
    end
  end
end

FizzBuzz.to(100)
