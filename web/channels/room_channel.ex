defmodule BlockParty.RoomChannel do
  use Phoenix.Channel
  alias BlockParty.SequencerState, as: SeqState

  def join("sequencers:lobby", _message, socket) do
    {:ok, socket}
  end
  def join("sequencers:" <> _private_room_id, _params, _socket) do
    {:error, %{ reason: "unknown room" }}
  end

  def handle_in(
    "set_cell",
    %{ "x" => x, "y" => y, "active" => active },
    socket
  ) when is_boolean(active) do
    grid = SeqState.set_cell(x, y, active)
    data = grid |> Tuple.to_list |> Enum.map(&Tuple.to_list/1)
    broadcast! socket, "grid", %{ grid: data }
    {:noreply, socket}
  end
end
