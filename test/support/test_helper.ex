defmodule TestHelper do
  def truncate_database(_) do
    {:ok, _} = Fcat.Neo4j.query("MATCH (n) DETACH DELETE n")
    :ok
  end
end
