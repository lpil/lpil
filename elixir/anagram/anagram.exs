defmodule Anagram do
  @doc """
  Returns all candidates that are anagrams of, but not equal to, 'base'.
  """
  @spec match(String.t, [String.t]) :: [String.t]
  def match(base, candidates) do
    charify = fn word ->
      String.downcase(word)
      |> String.graphemes
      |> Enum.sort
    end

    base = String.downcase(base)
    chars = charify.(base)

    Enum.reject(candidates, &(base === String.downcase(&1)))
    |> Enum.filter(&(chars === charify.(&1)))
  end
end
