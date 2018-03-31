defmodule Fcat.Neo4j do
  @moduledoc """
  Module for interacting with the Neo4j database, a wrapper around the
  Bolt.Sips driver. Not to be used directly by business logic code,
  instead they should use intermediate modules that handle querying.
  """

  alias Fcat.Metrics

  @type cypher :: String.t()

  @doc """
  Run a query against the database.

  If the params are a struct this function will attempt to validate
  it using `Vex.validate/1` and only run the query if valid.
  """
  @spec query(cypher, map) :: {:ok, Bolt.Sips.Response} | {:error, Bolt.Sips.Error}
  def query(cypher, params \\ %{})

  def query(cypher, params) when is_list(params) do
    query(cypher, Enum.into(params, %{}))
  end

  # If the params are a struct, we assume that the Vex validation
  # protocol has been implemented for it and check with that.
  def query(cypher, %{__struct__: _} = params) do
    case Vex.validate(params) do
      {:ok, _} -> run_query(cypher, params)
      {:error, errors} -> {:invalid, errors}
    end
  end

  def query(cypher, params) do
    run_query(cypher, params)
  end

  defp run_query(cypher, params) do
    Metrics.increment_counter("neo4j/query", 1)

    insert = fn ->
      case Bolt.Sips.query(Bolt.Sips.conn(), cypher, params) do
        {:ok, _} = result -> result
        {:error, error} -> normalise_error(error)
      end
    end

    Metrics.record_event("neo4j_query", cypher, insert)
  end

  # Some db errors (such as unique constraint errors) we wish to surface as
  # validation errors, so convert them to the Vex format.
  defp normalise_error(error) do
    case error[:code] do
      "Neo.ClientError.Schema.ConstraintValidationFailed" ->
        field =
          ~r/already exists.+property `(.+)` =/
          |> Regex.run(error[:message], captures: :first)
          |> List.last()
          |> String.to_existing_atom()

        {:invalid, [{:error, field, :uniqueness, "has already been taken"}]}

      _ ->
        {:error, error}
    end
  end

  def query_one(cypher, params \\ %{}) do
    case query(cypher, params) do
      {:ok, []} -> :not_found
      {:ok, [entity]} -> {:ok, entity}
      error -> error
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
