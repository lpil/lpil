defmodule ETL do
  @doc """
  Transform an index into an inverted index.

  ## Examples

  iex> ETL.transform(%{"a" => ["ABILITY", "AARDVARK"]}, "b" => ["BALLAST", "BEAUTY"]})
  %{"ability" => "a", "aardvark" => "a", "ballast" => "b", "beauty" =>"b"}
  """
  @spec transform(Dict.t) :: map() 
  def transform(input) do
    Enum.reduce input, %{}, &transformify(&1, &2)
  end

  defp transformify({score, words}, acc) do
    update = &Dict.put(&2, &1, score)

    words
    |> Enum.map(&String.downcase(&1))
    |> Enum.reduce(acc, update)
  end
end
