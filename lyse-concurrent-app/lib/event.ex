defmodule LYSE.Event do
  @moduledoc """
  A scheduled event in our reminder system.
  """

  alias __MODULE__

  defstruct server: nil,
            name:   nil,
            ms:     0
            
  def loop(state = %Event{ server: server }) do
    receive do
      {^server, reference, :cancel} ->
        send server, {reference, :ok}

      after state.ms ->
        send server, {:done, state.name}
    end
  end
end
