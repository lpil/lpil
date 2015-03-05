defmodule Sublist do
  @doc """
  Returns whether the first list is a sublist or a superlist of the second list
  and if not whether it is equal or unequal to the second list.
  """
  def compare(x, y) when x === y do
    :equal
  end

  def compare(x, y) when length(x) === length(y) do
    :unequal
  end

  def compare(x,y) when length(x) > length(y) do
    if superlist?(x, y) do
      :superlist
    else
      :unequal
    end
  end

  def compare(x,y) when length(x) < length(y) do
    if sublist?(x, y) do
      :sublist
    else
      :unequal
    end
  end

  defp superlist?(x, y) do
    sublist?(y, x)
  end

  defp sublist?(sub, _) when sub === [] do
    true
  end

  defp sublist?(sub, sup) do
    Stream.chunk(sup, length(sub), 1)
    |> Enum.any? &(&1 === sub)
  end
end
