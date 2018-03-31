defmodule Fcat.Application do
  @moduledoc """
  The entrypoint of the application. Here we define and start the supervision
  tree that comprises the running processes of the app.
  """
  use Application

  def start(_type, _args) do
    children = [
      # Neo4j database connection pool
      {Bolt.Sips, Application.get_env(:bolt_sips, Bolt)},
      # Web application endpoint
      FcatWeb.Endpoint
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Fcat.Supervisor)
  end

  def config_change(changed, _new, removed) do
    FcatWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
