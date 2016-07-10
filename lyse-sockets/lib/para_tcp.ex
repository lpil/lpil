defmodule ParaTCP do
  def start(port \\ 4000) do
    ParaTCP.Supervisor.start_link(port)
  end
end

defmodule ParaTCP.Supervisor do
  use Supervisor

  def start_link(port) do
    Supervisor.start_link(__MODULE__, port, name: __MODULE__)
  end

  def init(port) do
    {:ok, listen_socket} = :gen_tcp.listen(port, active: :once, packet: :line)
    start_listeners()
    children = [
      worker(ParaTCP.Listener, [listen_socket], []),
    ]
    supervise(children, strategy: :simple_one_for_one)
  end

  def start_listener do
    Supervisor.start_child(__MODULE__, [])
  end

  defp start_listeners do
    spawn_link fn->
      for _ <- 1..20, do: start_listener()
    end
  end
end

defmodule ParaTCP.Listener do
  require Logger
  use GenServer

  def start_link(socket) do
    GenServer.start_link(__MODULE__, socket)
  end

  def init(socket) do
    GenServer.cast(self(), :start_accepting)
    {:ok, socket}
  end

  def handle_cast(:start_accepting, socket) do
    {:ok, socket} = :gen_tcp.accept(socket)
    ParaTCP.Supervisor.start_listener()
    tcp_send(socket, "Hi! I'm going to echo anything you send.\n")
    {:noreply, socket}
  end

  def handle_info({:tcp, socket, 'quit' ++ _}, socket),
    do: close(socket)
  def handle_info({:tcp_closed, socket}, socket),
    do: close(socket)

  def handle_info({:tcp, socket, msg}, socket) do
    Logger.info "Got message from client"
    tcp_send(socket, msg)
    {:noreply, socket}
  end

  defp close(socket) do
    Logger.info "Client quit"
    :gen_tcp.close(socket)
    {:stop, :normal, socket}
  end

  defp tcp_send(socket, msg) do
    :ok = :gen_tcp.send(socket, msg)
    :ok = :inet.setopts(socket, active: :once)
    :ok
  end
end
