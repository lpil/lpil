defmodule BlockParty.SequencerState do
  @moduledoc """
  An Agent process that holds the mutable state of a sequencer grid.
  """

  @x_size 16
  @y_size 16

  row = Tuple.duplicate(false, @x_size)
  @default_state Tuple.duplicate(row, @y_size)

  @doc """
  Starts a new agent that holds the state of the sequencer.
  """
  def start_link do
    Agent.start_link(fn -> @default_state end)
  end
  def start_link({:global, true}) do
    Agent.start_link(fn -> @default_state end, name: __MODULE__)
  end

  @doc """
  Gets the current state of the sequencer grid.
  """
  def get_grid do
    get_grid(__MODULE__)
  end
  def get_grid(sequencer) do
    Agent.get(sequencer, fn x -> x end)
  end

  @doc """
  Set a cell in the sequencer, which is persisted in the Agent process.
  The entire grid is returned.
  """
  def set_cell(x, y, active) do
    set_cell(__MODULE__, x, y, active)
  end
  def set_cell(sequencer, x, y, active)
  when x >= 0 and x < @x_size
  and  y >= 0 and y < @y_size
  do
    Agent.get_and_update(sequencer, fn grid ->
      new_row  = grid |> elem(y) |> put_elem(x, active)
      new_grid = grid |> put_elem(y, new_row)
      {new_grid, new_grid}
    end)
  end
end
