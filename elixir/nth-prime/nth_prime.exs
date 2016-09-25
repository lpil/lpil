defmodule Prime do

  @doc """
  Generates the nth prime.
  """
  @spec nth(non_neg_integer) :: non_neg_integer
  def nth(count) when count > 0 do
    nth(count - 1, 3, [2])
  end

  defp nth(0, _, [nth_prime | _]) do
    nth_prime
  end

  defp nth(count, candidate, primes) do
    is_prime = ! Enum.any?(primes, &factor?(candidate, &1))
    if is_prime do
      nth(count - 1, candidate + 2, [candidate|primes])
    else
      nth(count, candidate + 2, primes)
    end
  end

  defp factor?(num, divider) do
    rem(num, divider) == 0
  end
end
