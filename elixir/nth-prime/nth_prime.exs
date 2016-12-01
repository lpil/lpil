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
    if prime?(candidate, primes) do
      nth(count - 1, candidate + 2, [candidate|primes])
    else
      nth(count, candidate + 2, primes)
    end
  end


  defp prime?(candidate, lesser_primes) do
    sqrt = :math.sqrt(candidate)
    prime?(candidate, lesser_primes, sqrt)
  end


  defp prime?(candidate, [], sqrt) do
    true
  end

  # Primes larger than the sqrt cannot be a factor.
  defp prime?(candidate, [largest_prime | lesser_primes], sqrt)
  when largest_prime > sqrt
  do
    prime?(candidate, lesser_primes, sqrt)
  end

  defp prime?(candidate, [largest_prime | lesser_primes], sqrt) do
    if factor?(candidate, largest_prime) do
      false
    else
      prime?(candidate, lesser_primes, sqrt)
    end
  end


  defp factor?(num, divider) do
    rem(num, divider) == 0
  end
end
