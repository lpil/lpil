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

  ## Server Callbacks
end
