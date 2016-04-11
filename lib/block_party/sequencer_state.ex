defmodule BlockParty.SequencerState do
  @moduledoc """
  An Agent process that holds the mutable state of a sequencer grid.
  """

  row = Tuple.duplicate(false, 8)
  @default_state Tuple.duplicate(row, 8)

  @doc """
  Starts a new agent that holds the state of the sequencer.
  """
  def start_link do
    Agent.start_link(fn -> @default_state end)
  end

  @doc """
  Gets a value from the `bucket` by `key`.
  """
  def get_grid(sequencer) do
    Agent.get(sequencer, fn x -> x end)
  end

  @doc """
  Set a cell in the sequencer, which is persisted in the Agent process.
  The entire grid is returned.
  """
  def set_cell(sequencer, x, y, value)
  when value in [true, false]
  and x >= 0 and x < 8
  and y >= 0 and y < 8
  do
    Agent.get_and_update(sequencer, fn grid ->
      new_row  = grid |> elem(y) |> put_elem(x, value)
      new_grid = grid |> put_elem(y, new_row)
      {new_grid, new_grid}
    end)
  end
end
