defmodule BlockParty.RoomChannel do
  use Phoenix.Channel

  def join("rooms:lobby", _message, socket) do
    {:ok, socket}
  end
  def join("rooms:" <> _private_room_id, _params, _socket) do
    {:error, %{ reason: "unknown room" }}
  end

  def handle_in("new_msg", %{ "body" => body }, socket) do
    broadcast! socket, "msg", %{ body: "someone sent #{body}" }
    {:noreply, socket}
  end
end
