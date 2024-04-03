defmodule NaiveTCP do
  def start_server(port) do
    pid = spawn_link fn->
      {:ok, listen_socket} = :gen_tcp.listen(port, [:binary, {:active, false}])
      spawn fn-> acceptor(listen_socket) end
      :timer.sleep(:infinity)
    end
    {:ok, pid}
  end

  defp acceptor(listen_socket) do
    {:ok, socket} = :gen_tcp.accept(listen_socket)
    spawn fn-> acceptor(listen_socket) end
    handle(socket)
  end

  defp handle(socket) do
    :inet.setopts(socket, active: :once)
    receive do
      {:tcp, ^socket, "quit" <> _} ->
        log("Client quit")
        :gen_tcp.close(socket)

      {:tcp, ^socket, msg} ->
        log("Got message '#{msg}' from client")
        :gen_tcp.send(socket, msg)
        handle(socket)
    end
  end

  defp log(msg) do
    IO.puts inspect(self()) <> ": " <> msg
  end
end
