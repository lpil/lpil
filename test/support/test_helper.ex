defmodule TestHelper do
  def truncate_database(_) do
    {:ok, _} = Orientdb.command("TRUNCATE CLASS User")
    :ok
  end
end
