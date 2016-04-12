defmodule BlockParty do
  @moduledoc """
  Collaberative music sequencing in the browser.
  """
  use Application

  alias BlockParty.{
    Endpoint,
    SequencerState,
  }

  def start(_type, _args) do
    import Supervisor.Spec, warn: false
    children = [
      supervisor(Endpoint, []),

      # This process holds the state of the sequencer.
      # Currently there is only one. Later if we wish to support multiple
      # sequencers at once we will need to instead use a process that spinsup
      # instances of this process, or to an ETS table.
      #
      # When we do either of these we will need to think carefully about how
      # we will remove old, unused sequencer state. If we do not expire this
      # data somehow the amount of memory we use will grow over time as new
      # sequencers are created.
      #
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
