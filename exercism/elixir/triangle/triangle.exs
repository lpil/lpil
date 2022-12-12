defmodule Triangle do
  @type kind :: :equilateral | :isosceles | :scalene

  @doc """
  Return the kind of triangle of a triangle with 'a', 'b' and 'c' as lengths.
  """
  @spec kind(number, number, number) :: { :ok, kind } | { :error, String.t }
  def kind(a, b, c) when a <= 0 or b <= 0 or c <= 0 do
    {:error, "all side lengths must be positive"}
  end

  def kind(a, b, c) when a <= 0 or b <= 0 or c <= 0 do
    {:error, "all side lengths must be positive"}
  end

  def kind(a, b, c) do
    [a, b, c] = Enum.sort([a, b, c])

    if a + b < c do
      {:error, "side lengths violate triangle inequality"}
    else
      type(a, b, c)
    end
  end

  defp type(a, b, c) when a === b and b === c do
    {:ok, :equilateral}
  end

  defp type(a, b, _) when a === b do
    {:ok, :isosceles}
  end

  defp type(_, a, b) when a === b do
    {:ok, :isosceles}
  end

  defp type(a, _, b) when a === b do
    {:ok, :isosceles}
  end

  defp type(_, _, _) do
    {:ok, :scalene}
  end
end
