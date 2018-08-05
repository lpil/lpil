defmodule Kitty do
  use Application

  defstruct name: "", colour: "", description: ""

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      worker(Kitty.Server, []),
    ]

    opts = [strategy: :one_for_one, name: Kitty.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
