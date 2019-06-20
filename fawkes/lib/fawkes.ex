defmodule Fawkes do
  @moduledoc """
  Dumbledore's pet phoenix.

  Also a blog web app.
  """
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      # Start the endpoint when the application starts
      supervisor(Fawkes.Endpoint, []),
      # Start the Ecto repository
      supervisor(Fawkes.Repo, []),
      # Here you could define other workers and supervisors as children
      # worker(Fawkes.Worker, [arg1, arg2, arg3]),
    ]

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Fawkes.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    Fawkes.Endpoint.config_change(changed, removed)
    :ok
  end
end
