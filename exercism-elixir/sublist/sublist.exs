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

  defp sublist?(sub, sup) do
    sub_l = length(sub)
    sup_l = length(sup)
    delta = sup_l - sub_l
    slice_super = &(Enum.slice sup, &1, sub_l)

    slices = Stream.map(0..delta, slice_super)

    Enum.any? slices, &(&1 === sub)
  end
end
