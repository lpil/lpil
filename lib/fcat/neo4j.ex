defmodule Fcat.Neo4j do
  @moduledoc """
  Module for interacting with the Neo4j database, a wrapper around the
  Bolt.Sips driver. Not to be used directly by business logic code,
  instead they should use intermediate modules that handle querying.
  """

  alias Fcat.Metrics

  @type cypher :: String.t()

  @spec query(cypher, map) :: {:ok, Bolt.Sips.Response} | {:error, Bolt.Sips.Error}
  def query(cypher, params) do
    Metrics.increment_counter("neo4j/query", 1)

    Metrics.record_event("neo4j_query", cypher, fn ->
      Bolt.Sips.query(Bolt.Sips.conn(), cypher, params)
    end)
  end
end
