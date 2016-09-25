defmodule BracketPush do
  @doc """
  Checks that all the brackets and braces in the string are matched correctly,
  and nested correctly
  """
  @spec check_brackets(String.t) :: boolean
  def check_brackets(brackets) do
    brackets
    |> String.graphemes
    |> check()
  end

  defp check(brackets, open_stack \\ [])

  defp check(["}"|cs], ["{"|stack]) do
    check(cs, stack)
  end

  defp check(["]"|cs], ["["|stack]) do
    check(cs, stack)
  end

  defp check([")"|cs], ["("|stack]) do
    check(cs, stack)
  end

  defp check([c|cs], stack) when c in ["}", "]", ")"] do
    check(cs, [c|stack])
  end

  defp check([c|cs], stack) when c in ["{", "[", "("] do
    check(cs, [c|stack])
  end

  defp check([_|cs], stack) do
    check(cs, stack)
  end

  defp check([], []), do: true
  defp check([], _), do: false
end
