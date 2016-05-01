defmodule Kitty.Server do
  use GenServer
  require Logger

  # Client API

  @doc """
  Synchronously order a cat.
  """
  def order_cat(name, colour, desc) do
    GenServer.call(__MODULE__, {:order, name, colour, desc})
  end

  @doc """
  Asynchronously return a cat.
  """
  def return_cat(%Kitty{} = cat) do
    GenServer.cast(__MODULE__, {:return, cat})
  end

  @doc """
  Synchronously close up the shop, releasing the cats.
  """
  def close_shop do
    GenServer.call(__MODULE__, :terminate)
  end


  # Server callbacks

  def start_link do
    GenServer.start_link(__MODULE__, [], [name: __MODULE__])
  end

  def handle_call({:order, n, c, d}, _from, []) do
    cat = %Kitty{ name: n, colour: c, description: d }
    {:reply, cat, state}
  end
  def handle_call({:order, n, c, d}, _from, [cat|cats]) do
    {:reply, cat, cats}
  end

  def handle_call(:terminate, _from, cats) do
    {:stop, :normal, :ok, cats}
  end

  def handle_cast({:return, %Kitty{} = cat}, state) do
   {:noreply, [cat|state]}
  end

  def handle_info(message, state) do
    Logger.info "Kitty.Server received unexpected message: #{inspect message}"
    {:noreply, state}
  end
end
