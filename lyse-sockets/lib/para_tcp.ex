defmodule ParaTCP.Supervisor do
  use Supervisor

  @port 4000
  @child_args [
  ]

  def start_link do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    {:ok, listen_socket} = :gen_tcp.listen(@port, active: :once, packet: :line)
    async_start_children()
    children = [
      worker(ParaTCP.Listener, [@child_args], []),
    ]
    supervise(children, strategy: :simple_one_for_one)
  end

  defp async_start_children do
    spawn_link fn->
      for i <- 1..20 do
        Supervisor.start_child(__MODULE__, [])
      end
    end
  end
end

defmodule ParaTCP.Listener do
  use GenServer

  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  def init(x) do
    IO.inspect x
    {:ok, x}
  end
end
