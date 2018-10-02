defmodule App do
  @moduledoc """
  An Elli handler module written in Elixir.
  """

  @behaviour :elli_handler

  # Elli handler callbacks

  @impl :elli_handler
  def handle(req, _args) do
    # Delegate to our handler function
    handle(:elli_request.method(req), :elli_request.path(req), req)
  end

  # Return 200 to GET requests
  defp handle(:GET, _, _req) do
    {200, [], "Hello, sailor!"}
  end

  # Return 404 to any other requests
  defp handle(_, _, _req) do
    {404, [], "Our princess is in another castle..."}
  end

  # Handle request events, like request completed, exception
  # thrown, client timeout, etc. Must return `ok'.
  @impl :elli_handler
  def handle_event(_event, _data, _args) do
    :ok
  end

  # Web server process creation functions

  @doc """
  Specification used to start the web server in a supervision tree.
  """
  def child_spec(args) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [args]}
    }
  end

  @doc """
  Start and link a new Elli web server using this handler.
  """
  def start_link(args) do
    :elli.start_link(
      port: args[:port],
      callback: __MODULE__,
      callback_args: args
    )
  end
end
