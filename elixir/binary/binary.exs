defmodule Binary do
  @doc """
  Convert a string containing a binary number to an integer.

  On errors returns 0.
  """
  @spec to_decimal(String.t) :: non_neg_integer
  def to_decimal(string) do
    if Regex.match?(~r/[^10]/, string) do
      0
    else
      convert string
    end
  end

  defp convert(string) do
    string
    |> String.graphemes
    |> Stream.map(&Integer.parse(&1))
    |> Enum.reduce(0, fn {x, _}, acc -> x + acc*2 end)
  end
end
