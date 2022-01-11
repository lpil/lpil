defmodule Exqy do
  def start_enqueuer do
    {:ok, pid} = Exq.Enqueuer.start_link([port: 6379])
  end

  def enqueue(data) do
    enqueuer = Exq.Enqueuer
    queue = "default"
    worker = Exqy.Worker
    {:ok, _ack} = Exq.enqueue(enqueuer, queue, worker, [data])
  end
end

defmodule Exqy.Worker do

  #
  # Callbacks
  #

  def perform(data) do
    IO.puts "Worker starting to process #{inspect data}"
    register_self()
    loop(data)
  end

  defp loop(data) do
    receive do
      :stop ->
        IO.puts "Worker got :stop. Stopping"

      :job_metadata ->
        job = Exq.worker_job()
        IO.puts "Worker got :job_metadata. Metadata is #{inspect job}"
        loop(data)


      msg ->
        IO.puts "Worker got #{inspect msg}. State is #{inspect data}"
        loop(data)
    end
    :ok
  end

  defp register_self do
    if Process.whereis(__MODULE__) == nil do
      true = Process.register(self(), __MODULE__)
    end
    :ok
  end
end
