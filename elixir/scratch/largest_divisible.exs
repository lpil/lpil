defmodule LargestDivisible do
  defp divisable?(num, divider) do
    rem(num, divider) == 0
  end

  @doc """
  Finds the largest number divisable by `divider` that is less than `num`
  """
  def find(num, divider) when divider > num do
    raise "divider larger than num"
  end
  def find(num, divider) do
    Enum.find (num - 1)..1,
              &divisable?(&1, divider)
  end
end
