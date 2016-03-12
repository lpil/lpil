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
        subscribe_client(state, pid, msg_ref, client)

      {pid, msg_ref, {:add, name, description, timeout}} ->
        add_event(state, pid, msg_ref, name, description, timeout)

      {pid, msg_ref, {:cancel, name}} ->
        cancel_event(state, pid, msg_ref, name)
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
        loop state
    end
  end

  defp subscribe_client(state, pid, msg_ref, client) do
    ref     = Process.monitor(client)
    clients = Map.put(state.clients, ref, client)
    state   = Map.put(state, :clients, clients)
    send pid, {msg_ref, :ok}
    loop state
  end

  defp add_event(state, pid, msg_ref, name, description, timeout) do
    event_pid = Event.start_link(name, timeout)
    event = %StoredEvent{
      name: name,     description: description,
      pid: event_pid, timeout: timeout,
    }
    new_events = %{ state.events | name => event }
    send pid, {msg_ref, :ok}
    loop(%{ state | events: new_events })
  end

  defp cancel_event(state, pid, msg_ref, name) do
    new_events = case Map.pop(name, state.events) do
      {nil, events} ->
        events

      {event, events} ->
        Event.cancel(event.pid)
        events
    end
    send pid, {msg_ref, :ok}
    loop %{ state | events: new_events }
  end
end
