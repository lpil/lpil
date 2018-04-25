defmodule Particle.Application do
  @moduledoc """
  The entrypoint of the application. Here we define and start the supervision
  tree that comprises the running processes of the app.
  """
  use Application

  def start(_type, _args) do
    children = [
      # OrientDB schema updater
      Particle.DbMigrator,
      # Web application endpoint
      ParticleWeb.Endpoint
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Particle.Supervisor)
  end

  def config_change(changed, _new, removed) do
    ParticleWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
