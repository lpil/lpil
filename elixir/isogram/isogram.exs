defmodule Isogram do
  @ignored_chars [" ", "-"]

  @doc """
  Determines if a word or sentence is an isogram
  """
  @spec isogram?(String.t) :: boolean
  def isogram?(sentence) do
    sentence
    |> String.graphemes
    |> no_duplicates?()
  end

  defp no_duplicates?(list, seen \\ MapSet.new)

  defp no_duplicates?([x|xs], seen) when x in @ignored_chars do
    no_duplicates?(xs, seen)
  end

  defp no_duplicates?([x|xs], seen) do
    if MapSet.member?(seen, x) do
      false
    else
      no_duplicates?(xs, MapSet.put(seen, x))
    end
  end

  defp no_duplicates?([], seen) do
    true
  end
end
