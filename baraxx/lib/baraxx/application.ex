defmodule Baraxx.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    ace_options = [
      port: 4000,
      # certfile: certfile,
      # keyfile: keyfile,
      connections: 1_000,
      # No TLS
      cleartext: true
    ]

    children = [
      {Ace.HTTP.Service, [{Baraxx.Web, []}, ace_options]}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Baraxx.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
