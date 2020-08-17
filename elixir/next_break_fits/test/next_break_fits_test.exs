defmodule NextBreakFitsTest do
  use ExUnit.Case

  import Inspect.Algebra

  defp render(doc, limit) do
    doc |> group() |> format(limit) |> IO.iodata_to_binary()
  end

  test "greets the world" do
    elems =
      break("")
      |> concat("1,")
      |> concat(break())
      |> concat("2,")
      |> concat(break())
      |> concat("3")
      |> group()
      |> nest(2)
      |> concat(break(""))

    list =
      "["
      |> concat(elems)
      |> concat("]")
      |> group()

    doc =
      "assert "
      |> concat(next_break_fits(list, :enabled))
      |> concat(" = ")
      |> concat(list)
      |> concat("\n")

    assert render(doc, 20) == """
           assert [1, 2, 3] = [
             1,
             2,
             3
           ]
           """

    assert render(doc, 10) == """
           assert [
             1,
             2,
             3
           ] = [
             1,
             2,
             3
           ]
           """

    assert render(doc, 30) == """
           assert [1, 2, 3] = [1, 2, 3]
           """
  end
end
