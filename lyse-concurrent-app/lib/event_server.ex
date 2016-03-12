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


  # Public API

  def start do
    pid = spawn __MODULE__, :init, []
    Process.register(pid, __MODULE__)
  end

  def start_link do
    pid = spawn_link __MODULE__, :init, []
    Process.register(pid, __MODULE__)
  end

  def init do
    # Load events from disk here.
    loop %__MODULE__{}
  end

  def terminate do
    send __MODULE__, :shutdown
  end

  def subscribe(client_pid) do
    ref = Process.monitor(client_pid)
    send __MODULE__, {self(), ref, {:subscribe, client_pid}}
    receive do
      {ref, :ok} ->
        {ref, :ok}

      {:DOWN, _ref, :process, _pid, reason} ->
        {:error, reason}

    after 5000 ->
      {:error, :timeout}
    end
  end


  # Server functions

  defp loop(state = %__MODULE__{}) do
    receive do
      {pid, msg_ref, {:subscribe, client}} ->
        subscribe_client(state, pid, msg_ref, client)

      {pid, msg_ref, {:add, name, description, timeout}} ->
        add_event(state, pid, msg_ref, name, description, timeout)

      {pid, msg_ref, {:cancel, name}} ->
        cancel_event(state, pid, msg_ref, name)

      {:done, name} ->
        handle_event_done(state, name)

      :shutdown ->
        exit :shutdown

      {'DOWN', ref, :process, _pid, _reason} ->
        events = Map.delete(state.events, ref)
        loop %{ state | events: events }

      :code_change ->
        __MODULE__.loop state

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

  defp handle_event_done(state, name) do
    case Map.pop(name, state.events) do
      {nil, _events} ->
        # We cancelled the event at the same time it finished
        loop state

      {event, events} ->
        Enum.each(state.clients, fn {_ref, pid} ->
          send pid, {:done, event.name, event.description}
        end)
        loop %{ state | events: events }
    end
  end
end
