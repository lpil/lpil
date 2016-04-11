defmodule BlockParty do
  use Application

  alias BlockParty.{
    Endpoint,
    SequencerState,
  }

  def start(_type, _args) do
    import Supervisor.Spec, warn: false
    children = [
      supervisor(Endpoint, []),
      worker(SequencerState, [global: true]),
    ]
    opts = [
      strategy: :one_for_one,
      name: BlockParty.Supervisor
    ]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    BlockParty.Endpoint.config_change(changed, removed)
    :ok
  end
end
