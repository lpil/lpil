defmodule BlockParty.RoomChannel do
  use Phoenix.Channel

  def join("rooms:lobby", _message, socket) do
    {:ok, socket}
  end
  def join("rooms:" <> _private_room_id, _params, _socket) do
    {:error, %{ reason: "unknown room" }}
  end

  def handle_in("set_cell", %{ "x" => x, "y" => y, "state" => state } = params, socket)
  when state in ["on", "off"] do
    broadcast! socket, "set_cell", params # Return payload to show we can.
    {:noreply, socket}
  end
end
