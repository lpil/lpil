defmodule NextBreakFitsTest do
  use ExUnit.Case

  import NextBreakFits

  defp render(doc, limit) do
    doc |> group() |> format(limit) |> IO.iodata_to_binary()
  end

  test "greets the world" do
    elems =
      break("")
      |> concat("1")
      |> nest(2)
      |> concat(break(""))

    list =
      "["
      |> concat(elems)
      |> concat("]")
      |> group()

    doc =
      list
      |> next_break_fits()
      |> concat(" = ")
      |> concat(list)
      |> concat("\n")

    assert render(doc, 7) == """
           [1] = [
             1
           ]
           """

    assert render(doc, 2) == """
           [
             1
           ] = [
             1
           ]
           """

    assert render(doc, 16) == """
           [1] = [1]
           """
  end
end
