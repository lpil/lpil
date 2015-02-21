# The Registry process maintains the mappings between bucket names and bucket
# processes, so that users can pass a name to access their desired bucket.

defmodule FirstApp.Registry do
  use GenServer

  ## Client API

  @doc """
  Start the registry
  """
  def start_link(event_manager, opts \\ []) do
    GenServer.start_link(__MODULE__, event_manager, opts)
  end

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

  # callback for start_link
  def init(events) do
    names = HashDict.new
    refs  = HashDict.new
    {:ok, %{names: names, refs: refs, events: events}}
  end

  # callback for lookup
  def handle_call({:lookup, name}, _from, state) do
    {:reply, HashDict.fetch(state.names, name), state}
  end

  # callback for stop
  def handle_call(:stop, _from, state) do
    {:stop, :normal, :ok, state}
  end


  # callback for create
  def handle_cast({:create, name}, state) do
    if HashDict.has_key?(state.names, name) do
      {:noreply, state}
    else
      {:ok, pid} = FirstApp.Bucket.start_link() # <- Bad idea. Registry will
      ref   = Process.monitor(pid)              # crash when the Bucket crashes
      refs  = HashDict.put(state.refs, ref, name)
      names = HashDict.put(state.names, name, pid)

      GenEvent.sync_notify(state.events, {:create, name, pid})
      {:noreply, %{state | names: names, refs: refs}}
    end
  end


  # callback for other messages, i.e. from Process.monitor
  def handle_info({:DOWN, ref, :process, pid, _reason}, state) do
    {name, refs} = HashDict.pop(state.refs, ref)
    names = HashDict.delete(state.names, name)

    GenEvent.sync_notify(state.events, {:exit, name, pid})
    {:noreply, %{state | names: names, refs: refs}}
  end

  # We need this catch all info handler as otherwise unexpected info messages
  # could crash the supervisor (Registry) due to no clause matching.
  def handle_info(_msg, state) do
    {:noreply, state}
  end
end
