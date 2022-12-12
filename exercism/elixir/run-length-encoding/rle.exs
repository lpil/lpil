defmodule RunLengthEncoder do
  @spec encode(String.t) :: String.t
  def encode(string) do
    string
    |> String.graphemes
    |> Enum.chunk_by(&indentity/1)
    |> Enum.map(fn(chars) -> "#{length(chars)}#{hd(chars)}" end)
    |> Enum.join
  end

  @spec decode(String.t) :: String.t
  def decode(string) do
    ~r/(\d+)(.)/
    |> Regex.scan(string)
    |> Enum.map(&expand_pair/1)
    |> Enum.join
  end

  defp indentity(x), do: x

  defp expand_pair([_, len, char]) do
    {num, _} = Integer.parse(len)
    String.duplicate(char, num)
  end
end
