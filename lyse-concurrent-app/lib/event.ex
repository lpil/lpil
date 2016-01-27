defmodule LYSE.Event do
  @moduledoc """
  A scheduled event process in our reminder system.
  """

  alias __MODULE__

  defstruct server: nil,
            name:   nil,
            ms:     0


  def start(event_name, delay_ms) do
    tate = %__MODULE__{ server: self(), name: event_name, ms: delay_ms }
    spawn fn-> loop(state) end
  end

  def start_link(event_name, delay_ms) do
    state = %__MODULE__{ server: self(), name: event_name, ms: delay_ms }
    spawn_link fn-> loop(state) end
  end

  def cancel(pid) do
    # Monitor in case the process is already dead
    ref = Process.monitor(pid)
    send pid, {self(), ref, :cancel}
    receive do
      {^ref, :ok} ->
        Process.demonitor(ref, [:flush])
        :ok

      {:DOWN, ^ref, :process, ^pid, _reason} ->
        :ok
    end
  end


  defp loop(state = %Event{ server: server }) do
    receive do
      {^server, reference, :cancel} ->
        send server, {reference, :ok}

      after state.ms ->
        send server, {:done, state.name}
      end
  end
end
