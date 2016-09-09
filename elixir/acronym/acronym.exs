defmodule Acronym.Codepoint do
  @capital_a 65
  @capital_z 90

  defmacro is_uppercase(c) do
    quote do
      unquote(c) >= unquote(@capital_a)
      and unquote(c) <= unquote(@capital_z)
    end
  end
end

defmodule Acronym do
  alias Acronym.Codepoint
  require Codepoint

  @doc """
  Generate an acronym from a string.
  "This is a string" => "TIAS"
  """
  def abbreviate(string, acc \\ "")

  def abbreviate("", acc) do
    acc
  end

  def abbreviate(<<" "::utf8, c::utf8, rest::binary>>, acc) do
    acc = acc <> String.upcase(<<c>>)
    abbreviate(rest, acc)
  end

  def abbreviate(<<c::utf8, rest::binary>>, acc)
  when Codepoint.is_uppercase(c)
  do
    abbreviate(rest, acc <> <<c>>)
  end

  def abbreviate(<<_::utf8, rest::binary>>, acc) do
    abbreviate(rest, acc)
  end
end
