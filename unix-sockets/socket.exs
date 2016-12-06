path = '/tmp/my-socket'

{:ok, socket} = :gen_tcp.connect({:local, path}, 0, [:binary])
:ok = :gen_tcp.send(socket, "Some Data")

combinator = fn f ->
  (fn z ->
    z.(z)
  end).(fn x ->
          f.(fn y -> (x.(x)).(y) end)
  end)
end

loop = fn(loop)->
  fn(:ok) ->
    receive do
      x ->
        IO.inspect x
        Process.sleep(100)
        :ok = :gen_tcp.send(socket, "Hello from Elixir. #{inspect :erlang.monotonic_time}")
        loop.(:ok)
    end
  end
end

combinator.(loop).(:ok)

:ok = :gen_tcp.close(socket)
