defmodule SumOfMultiples do
  @doc """
  Adds up all numbers from 1 to a given end number that are multiples of the
  factors provided.
  """
  @spec to(non_neg_integer, [non_neg_integer]) :: non_neg_integer
  def to(limit, factors) do
    1..(limit - 1)
    |> Enum.filter(&factor_of_any?(&1, factors))
    |> Enum.sum
  end

  defp factor?(num, divider) do
    rem(num, divider) == 0
  end

  defp factor_of_any?(num, dividers) do
    Enum.any?(dividers, &factor?(num, &1))
  end
end
