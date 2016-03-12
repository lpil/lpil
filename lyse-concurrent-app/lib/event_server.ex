defmodule LYSE.EventServer do
  @moduledoc """
  Communicated to with Clients. Handles Events.
  """

  defmodule StoredEvent do
    @moduledoc false
    defstruct name: "", description: "", pid: nil, timeout: 0
  end

  alias LYSE.Event
  alias LYSE.EventServer.StoredEvent

  defstruct clients: %{},
            events:  %{}


  def loop(state = %__MODULE__{}) do
    receive do
      {pid, msg_ref, {:subscribe, client}} ->
        handle_subscribe(state, pid, msg_ref, client)

      {pid, msg_ref, {:add, name, description, timeout}} ->
        handle_add_event(state, pid, msg_ref, name, description, timeout)

      {pid, msg_ref, {:cancel, name}} ->
        nil

      {:done, name} ->
        nil

      :shutdown ->
        nil

      {'DOWN', ref, process, _pid, _reason} ->
        nil

      :code_change ->
        nil

      unknown ->
        IO.puts "Unknown message: #{unknown}"
        loop( state )
    end
  end

  defp handle_subscribe(state, pid, msg_ref, client) do
    ref     = Process.monitor(client)
    clients = Map.put(state.clients, ref, client)
    state   = Map.put(state, :clients, clients)
    send pid, {msg_ref, :ok}
    loop(state)
  end

  defp handle_add_event(state, pid, msg_ref, name, description, timeout) do
    event_pid = Event.start_link(name, timeout)
    event = %StoredEvent{
      name: name,     description: description,
      pid: event_pid, timeout: timeout,
    }
    new_events = %{ state.events | name => event }
    send pid, {msg_ref, :ok}
    loop(%{ state | events: new_events })
  end
end
