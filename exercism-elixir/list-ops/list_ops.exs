defmodule ListOps do
  # Please don't use any external modules (especially List) in your
  # implementation. The point of this exercise is to create these basic
  # functions yourself.
  #
  # Note that `++` is a function from an external module (Kernel, which is
  # automatically imported) and so shouldn't be used either.

  @spec count(list) :: non_neg_integer
  def count([]) do
    0
  end
  def count([x|xs]) do
    1 + count(xs)
  end

  @spec reverse(list) :: list
  def reverse(xs) do
    reduce xs, [], &([&1|&2])
  end

  @spec map(list, (any -> any)) :: list
  def map([], _) do
    []
  end
  def map([x|xs], f) do
    [f.(x) | map(xs, f)]
  end

  @spec filter(list, (any -> as_boolean(term))) :: list
  def filter([], _) do
    []
  end
  def filter([x|xs], f) do
    if f.(x) do
      [x | filter(xs, f)]
    else
      filter(xs, f)
    end
  end

  @type acc :: any
  @spec reduce(list, acc, ((any, acc) -> acc)) :: acc
  def reduce([], acc, _) do
    acc
  end
  def reduce([x|xs], acc, f) do
    reduce xs, f.(x, acc), f
  end

  @spec append(list, list) :: list
  def append([], b) do
    b
  end
  def append([a|as], b) do
    [a | append(as, b)]
  end

  @spec concat([[any]]) :: [any]
  def concat(xs) do
    concat reverse(xs), []
  end
  defp concat([], acc) do
    acc
  end
  defp concat([x|xs], acc) do
    concat xs, append(x, acc)
  end
end
