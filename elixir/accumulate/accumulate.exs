defmodule Accumulate do
  @doc """
    Given a list and a function, apply the function to each list item and
    replace it with the function's return value.

    Returns a list.

    ## Examples

      iex> Accumulate.accumulate([], fn(x) -> x * 2 end)
      []

      iex> Accumulate.accumulate([1, 2, 3], fn(x) -> x * 2 end)
      [2, 4, 6]

  """

  @spec accumulate(list, (any -> any)) :: list
  def accumulate([], _) do
    []
  end
  def accumulate([x|xs], fun) do
    [fun.(x) | accumulate(xs, fun)]
  end

  # Tail recursive definition.
  # This one is slower, but more memory efficient.

  @spec tail_accumulate(list, (any -> any)) :: list
  def tail_accumulate(xs, fun) do
    tail_accumulate(xs, fun, [])
  end
  def tail_accumulate([], _, acc) do
    Enum.reverse(acc)
  end
  def tail_accumulate([x|xs], fun, acc) do
    tail_accumulate(xs, fun, [fun.(x) | acc])
  end
end
