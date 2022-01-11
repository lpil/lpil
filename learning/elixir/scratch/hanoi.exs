defmodule Hanoi do
  @doc """
  Solve the towers of Hanoi, moving rings from `a` to `b`
  """
  def solve(_, a \\ "a", b \\ "b", c \\ "c")
  def solve(0, _, _, _) do
    []
  end

  def solve(tower_height, a, b, c) do
    x = tower_height - 1

    solve(x, a, c, b) ++
    ["Move one ring from #{a} to #{b}"] ++
    solve(x, c, b, a)
  end
end

Hanoi.solve(3)
