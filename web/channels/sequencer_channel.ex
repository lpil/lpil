defmodule BlockParty.SequencerChannel do
  @moduledoc """
  Synchronisation of sequencers across multiple users.
  """
  use Phoenix.Channel
  alias BlockParty.SequencerState, as: SeqState

  def join("sequencers:lobby", _message, socket) do
    send self, :after_join
    {:ok, socket}
  end
  def join("sequencers:" <> _private_room_id, _params, _socket) do
    {:error, %{ reason: "unknown room" }}
  end

  # Send current grid state after a user joins.
  def handle_info(:after_join, socket) do
    grid = SeqState.get_grid |> grid_to_list
    push socket, "grid", %{ grid: grid }
    {:noreply, socket}
  end


  def handle_in(
    "set_cell",
    %{ "x" => x, "y" => y, "active" => active },
    socket
  ) do
    grid = SeqState.set_cell(x, y, active)
    data = grid |> grid_to_list
    broadcast! socket, "grid", %{ grid: data }
    {:noreply, socket}
  end

  def handle_in("set_bpm", bpm, socket) when is_integer(bpm) do
    grid = SeqState.set_bpm(bpm)
    broadcast! socket, "bpm", %{ bpm: bpm }
    {:noreply, socket}
  end


  defp grid_to_list(grid) do
    grid |> Tuple.to_list |> Enum.map(&Tuple.to_list/1)
  end
end
