defmodule Grains do
  @doc """
  Calculate two to the power of the input minus one.
  """
  @spec square(pos_integer) :: pos_integer
  def square(number) do
    hd(squares number)
  end

  defp squares(number) do
    Stream.iterate(1, &(&1 * 2))
    |> Stream.take(number)
    |> Enum.drop(number - 1)
  end

  @doc """
  Adds square of each number from 1 to 64.
  """
  @spec total :: pos_integer
  def total do
    Enum.sum(squares 65) - 1
  end
end
