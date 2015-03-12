defmodule PrimeFactors do
  @doc """
  Compute the prime factors for 'number'.

  The prime factors are prime numbers that when multiplied give the desired
  number.

  The prime factors of 'number' will be ordered lowest to highest. 
  """
  @spec factors_for(pos_integer) :: [pos_integer]
  def factors_for(n) do
    top = Enum.max([div(n, 2), 2]) + 1
    factors_for(n, [], primes(top))
  end

  defp factors_for(_, acc, []) do Enum.reverse acc end
  defp factors_for(1, acc, _ ) do Enum.reverse acc end

  defp factors_for(n, acc, [p|ps] = primes) do
    case rem(n, p) do
      0 ->
        factors_for(div(n, p), [p|acc], primes)
      _ ->
        factors_for(n, acc, ps)
    end
  end


  @doc """
  Computes a list of primes below n
  """
  @spec primes(pos_integer) :: [pos_integer]

  def primes(n) do
    nums = Enum.to_list(2..n)
    primes(nums, [])
  end

  defp primes([x|xs], ps) do
    x_is_prime = not Enum.any?(ps, &(rem(x, &1) == 0))

    if x_is_prime, do: ps = [x|ps]

    primes(xs, ps)
  end

  defp primes([], ps) do
    Enum.reverse ps
  end
end
