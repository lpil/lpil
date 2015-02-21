# The Registry process maintains the mappings between bucket names and bucket
# processes, so that users can pass a name to access their desired bucket.

defmodule FirstApp.Registry do
  use GenServer

  ## Client API

  @doc """
  Start the registry
  """
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, opts) # __MODULE__ is the current
  end                                           # module. Registry here.

  @doc """
  Look up the bucket PID for `name` in `server`.

  Returns `{:ok, pid}` if the bucket exists, `:error` otherwise.
  """
  def lookup(server, name) do
    GenServer.call(server, {:lookup, name}) # Calls are synchronous and return
  end

  @doc """
  Ensures that there is a bucket in `server` with the name `name`
  """
  def create(server, name) do
    GenServer.cast(server, {:create, name}) # Casts are async and don't return
  end

  @doc """
  Stop the Registry
  """
  def stop(server) do
    GenServer.call(server, :stop)
  end


  ##
  ## Server Callbacks
  ##  => Run on the GenServer
  ##

  def init(:ok) do # callback for start_link
    names = HashDict.new
    refs  = HashDict.new
    {:ok, {names, refs}}
  end

  # callback for lookup
  def handle_call({:lookup, name}, _from, {names, _} = state) do
    {:reply, HashDict.fetch(names, name), state}
  end

  # callback for stop
  def handle_call(:stop, _from, state) do
    {:stop, :normal, :ok, state}
  end


  # callback for create
  def handle_cast({:create, name}, {names, refs} = state) do
    if HashDict.has_key?(names, name) do
      {:noreply, state}
    else
      {:ok, pid} = FirstApp.Bucket.start_link()
      ref   = Process.monitor(pid)
      refs  = HashDict.put(refs, ref, name)
      names = HashDict.put(names, name, pid)
      {:noreply, {names, refs}}
    end
  end

  # callback for other messages, i.e. from Process.monitor
  def handle_info({:DOWN, ref, :process, _pid, _reason}, {names,refs}) do
    {name, refs} = HashDict.pop(refs, ref)
    names = HashDict.delete(names, name)
    {:noreply, {names, refs}}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
end
