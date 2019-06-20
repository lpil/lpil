defmodule BlockParty.SequencerStateTest do
  use ExUnit.Case
  alias BlockParty.SequencerState, as: SeqState

  test "get_grid/1 has default state" do
    {:ok, pid} = SeqState.start_link
    assert SeqState.get_grid(pid) == {
      {false, false, false, false, false, false, false, false},
      {false, false, false, false, false, false, false, false},
      {false, false, false, false, false, false, false, false},
      {false, false, false, false, false, false, false, false},
      {false, false, false, false, false, false, false, false},
      {false, false, false, false, false, false, false, false},
      {false, false, false, false, false, false, false, false},
      {false, false, false, false, false, false, false, false},
    }
  end

  test "cells can be set with set_cell/2" do
    {:ok, pid} = SeqState.start_link
    SeqState.set_cell(pid, 0, 0, true)
    grid = SeqState.set_cell(pid, 7, 1, true)
    assert grid == {
      {true,  false, false, false, false, false, false, false},
      {false, false, false, false, false, false, false, true },
      {false, false, false, false, false, false, false, false},
      {false, false, false, false, false, false, false, false},
      {false, false, false, false, false, false, false, false},
      {false, false, false, false, false, false, false, false},
      {false, false, false, false, false, false, false, false},
      {false, false, false, false, false, false, false, false},
    }
    assert SeqState.get_grid(pid) == grid
  end
end
