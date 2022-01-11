defmodule Web.Application do
  @moduledoc false

  use Application
  require Logger

  def start(_type, _args) do
    [
      web_server()
    ]
    |> Enum.filter(& &1)
    |> Supervisor.start_link(strategy: :one_for_one, name: __MODULE__)
  end

  defp web_server do
    if Application.get_env(:web, :enable_web_server) do
      ace_options = [
        port: 4000,
        # certfile: certfile,
        # keyfile: keyfile,
        connections: 1_000,
        # No TLS
        cleartext: true
      ]

      {Ace.HTTP.Service, [{Web, []}, ace_options]}
    end
  end
end
