defmodule Fcat.Neo4j do
  @moduledoc """
  Module for interacting with the Neo4j database, a wrapper around the
  Bolt.Sips driver. Not to be used directly by business logic code,
  instead they should use intermediate modules that handle querying.
  """

  alias Fcat.Metrics

  @type cypher :: String.t()

  @spec query(cypher, map) :: {:ok, Bolt.Sips.Response} | {:error, Bolt.Sips.Error}
  def query(cypher, params \\ %{})

  def query(cypher, params) when is_list(params) do
    query(cypher, Enum.into(params, %{}))
  end

  def query(cypher, params) do
    Metrics.increment_counter("neo4j/query", 1)

    Metrics.record_event("neo4j_query", cypher, fn ->
      Bolt.Sips.query(Bolt.Sips.conn(), cypher, params)
    end)
  end

  def query_one(cypher, params \\ %{}) do
    case query(cypher, params) do
      {:ok, []} -> :not_found
      {:ok, [entity]} -> {:ok, entity}
      {:error, _} = error -> error
    end
  end

  @doc """
  Ping the database.
  """
  @spec ping?() :: boolean
  def ping? do
    match?({:ok, _}, query("RETURN 1"))
  end

  @spec properties_to_struct(map, atom) :: map
  def properties_to_struct(%{properties: props}, struct_atom) do
    atom_props = Enum.map(props, fn {k, v} -> {String.to_existing_atom(k), v} end)
    struct(struct_atom, atom_props)
  end
end
