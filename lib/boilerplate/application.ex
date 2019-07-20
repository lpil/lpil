defmodule Boilerplate.Application do
  @moduledoc """
  The entrypoint of the application. Here we define and start the supervision
  tree that comprises the running processes of the app.
  """
  use Application

  def start(_type, _args) do
    children = [
      # Database connection pool
      Boilerplate.Repo,
      # Web application endpoint
      BoilerplateWeb.Endpoint
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Boilerplate.Supervisor)
  end

  def config_change(changed, _new, removed) do
    BoilerplateWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
