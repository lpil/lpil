{:ok, agent} = Agent.start_link(fn -> Stream.iterate 1, &(&1 * 2) end)
Process.register agent, :grains

defmodule Grains do
  @doc """
  Calculate two to the power of the input minus one.
  """
  @spec square(pos_integer) :: pos_integer
  def square(number) do
    squares(number)
    |> Enum.drop(number - 1)
    |> hd
  end

  defp squares(number) do
    grains = Agent.get(:grains, &(&1))

    answer = grains
    |> Stream.take(number)
    
    Agent.update :grains, fn _ -> grains end
    answer
  end

  @doc """
  Adds square of each number from 1 to 64.
  """
  @spec total :: pos_integer
  def total do
    IO.inspect squares(65)
    Enum.sum(squares 65) - 1
  end
end
