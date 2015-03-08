defmodule Teenager do
  def hey(input) do
    cond do
      Regex.match? ~r/^ *$/, input ->
        "Fine. Be that way!"

      (! Regex.match? ~r/^[\d\W]+$/, input) and
      String.upcase(input) == input ->
        "Whoa, chill out!"

      String.last(input) == "?" ->
        "Sure."

      true ->
        "Whatever."
    end
  end
end
