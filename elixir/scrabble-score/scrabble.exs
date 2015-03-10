defmodule Scrabble do
  @doc """
  Calculate the scrabble score for the word.
  """
  @spec score(String.t) :: non_neg_integer
  def score("") do
    0
  end

  def score(word) do
    word
    |> String.upcase
    |> String.replace(~r/[^A-Z]/, "")
    |> String.graphemes
    |> Stream.map(&value(&1))
    |> Enum.sum
  end

  defp value(letter) do
    case letter do
      "Q" -> 10
      "Z" -> 10
      "J" ->  8
      "X" ->  8
      "K" ->  5
      "F" ->  4
      "H" ->  4
      "V" ->  4
      "W" ->  4
      "Y" ->  4
      "B" ->  3
      "C" ->  3
      "M" ->  3
      "P" ->  3
      "D" ->  2
      "G" ->  2
       _  ->  1
    end
  end
end
