defmodule BlockParty.SequencerState do
  @moduledoc """
  An Agent process that holds the mutable state of a sequencer grid.
  """
  @x_size 16
  # @y_size 16
  @y_size 8

  row = Tuple.duplicate(false, @x_size)
  grid = Tuple.duplicate(row, @y_size)

  defstruct grid: grid,
            bpm:  130


  @doc """
  Starts a new agent that holds the state of the sequencer.
  """
  def start_link do
    Agent.start_link(fn -> %__MODULE__{} end)
  end
  def start_link({:global, true}) do
    Agent.start_link(fn -> %__MODULE__{} end, name: __MODULE__)
  end

  @doc """
  Gets the current state of the sequencer grid.
  """
  def get_grid do
    get_grid(__MODULE__)
  end
  def get_grid(sequencer) do
    Agent.get(sequencer, fn state -> state.grid end)
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
    Agent.get_and_update(sequencer, fn state ->
      new_row   = state.grid |> elem(y) |> put_elem(x, active)
      new_grid  = state.grid |> put_elem(y, new_row)
      new_state = %{ state | grid: new_grid }
      {new_grid, new_state}
    end)
  end

  # TODO: test
  @doc """
  Gets the current BPM of the sequencer.
  """
  def get_bpm do
    get_bpm(__MODULE__)
  end
  def get_bpm(sequencer) do
    Agent.get(sequencer, fn state -> state.bpm end)
  end


  # TODO: test
  @doc """
  Add a value to the BPM of the sequencer to get the new BPM.
  """
  def add_bpm(bpm) do
    add_bpm(__MODULE__, bpm)
  end
  def add_bpm(sequencer, bpm) when is_integer(bpm) do
    Agent.get_and_update(sequencer, fn state ->
      new_bpm   = state.bpm + bpm
      new_state = %{ state | bpm: new_bpm }
      {new_bpm, new_state}
    end)
  end
end
