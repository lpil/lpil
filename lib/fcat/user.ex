defmodule Fcat.User do
  @moduledoc """
  A user of the system. How exotic!
  """

  alias Fcat.{Metrics, Neo4j}

  defstruct [:id, :email, :inserted_at]

  @type t :: %__MODULE__{}

  @doc """
  Look up the user with the given id in the database.
  """
  @spec fetch(String.t()) :: {:ok, t} | :not_found
  def fetch(id) do
    cypher = """
    MATCH (user:User {id: $id})
    RETURN user
    LIMIT 1
    """

    with {:ok, data} <- Neo4j.query_one(cypher, id: id) do
      data
      |> from_result()
      |> Term.ok()
    end
  end

  # Validation of insert params
  defmodule Insert do
    @moduledoc false
    use Vex.Struct

    @enforce_keys [:id, :email]
    defstruct [:id, :email]

    validates(:id, presence: true)
    validates(:email, presence: true, format: ~r/.@.+\../)
  end

  @doc """
  Insert a new user into the database.
  """
  def insert(%Insert{} = params) do
    cypher = """
    CREATE (user:User {
      email: $email,
      id: $id,
      inserted_at: timestamp()
    })
    RETURN user
    """

    with {:ok, data} <- Neo4j.query_one(cypher, params) do
      Metrics.increment_counter("user/insert")

      data
      |> from_result()
      |> Term.ok()
    end
  end

  @doc """
  Insert a new User into the database, fetching it there already is one
  with the given id.
  """
  def fetch_or_insert(%Insert{} = params) do
    with :not_found <- fetch(params.id) do
      insert(params)
    end
  end

  defp from_result(data) do
    data
    |> Map.fetch!("user")
    |> Neo4j.properties_to_struct(__MODULE__)
  end
end
