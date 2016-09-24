defmodule Raindrops do
  @rules [
    {3, "Pling"},
    {5, "Plang"},
    {7, "Plong"},
  ]

  @doc """
  Returns a string based on raindrop factors.
  """
  def convert(number) do
    @rules
    |> Enum.reduce("", fn
      ({divider, word}, acc) when rem(number, divider) == 0 -> acc <> word
      (_, acc) -> acc
    end)
    |> case do
        "" -> to_string(number)
        x -> x
      end
  end
end
