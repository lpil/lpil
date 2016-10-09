#
# The manager recieves a call and delegates the replying to another process
# so it can respond to other requests.
#
defmodule Del do
  use Application

  def hello do
    IO.puts "#{__MODULE__} (#{inspect self()}) casting :hello to Del.Manager."
    response = GenServer.call(Del.Manager, :hello)
    IO.puts "#{__MODULE__} (#{inspect self()}) got response #{inspect response}"
    response
  end

  def start(_type, _args) do
    import Supervisor.Spec, warn: false
    children = [
      worker(Del.WorkerSupervisor, []),
      worker(Del.Manager, []),
    ]
    opts = [strategy: :one_for_one, name: Del.Supervisor]
    Supervisor.start_link(children, opts)
  end
end

defmodule Del.Manager do
  use GenServer

  def start_link,
    do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  def handle_call(:hello, from, state) do
    IO.puts "#{__MODULE__} (#{inspect self()}) got cast :hello from #{inspect from}."
    IO.puts "#{__MODULE__} (#{inspect self()}) delegating to worker. Not replying."
    Del.WorkerSupervisor.start_child(from)
    {:noreply, state}
  end
end

defmodule Del.WorkerSupervisor do
  def start_link do
    import Supervisor.Spec, warn: false
    Supervisor.start_link(
      [worker(Del.Worker, [], restart: :transient)],
      strategy: :simple_one_for_one, name: __MODULE__)
  end

  def start_child(target) do
    IO.puts "#{__MODULE__} starting child with state #{inspect target}."
    {:ok, _pid} = Supervisor.start_child(__MODULE__, [target])
  end
end

defmodule Del.Worker do
  use GenServer

  def start_link(state),
    do: GenServer.start_link(__MODULE__, state)

  def init(state) do
    IO.puts "#{__MODULE__} (#{inspect self()}) worker starting with immediate timeout."
    {:ok, state, 0}
  end

  def handle_info(:timeout, target) do
    IO.puts "#{__MODULE__} (#{inspect self()}) got timeout. Replying to #{inspect target}"
    GenServer.reply(target, {:hello, self()})
    {:stop, :normal, []}
  end
end
