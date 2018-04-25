defmodule Particle.User do
  @moduledoc """
  A user of the system. How exotic!
  """

  alias Particle.Metrics

  defstruct [:id, :email, :inserted_at]

  @type t :: %__MODULE__{}

  @doc """
  Look up the user with the given id in the database.
  """
  @spec fetch(String.t()) :: {:ok, t} | :not_found
  def fetch(id) do
    sql = """
    SELECT FROM User
    WHERE id = :id
    """

    case Orientdb.command(sql, id: id) do
      {:ok, %{"result" => [record]}} ->
        record
        |> Term.parse_struct(__MODULE__)
        |> Term.tag(:ok)

      {:ok, %{"result" => []}} ->
        :not_found
    end
  end

  @doc """
  Look up the user with the given email in the database.
  """
  @spec fetch_by_email(String.t()) :: {:ok, t} | :not_found
  def fetch_by_email(email) do
    sql = """
    SELECT FROM User
    WHERE email = :email
    """

    case Orientdb.command(sql, email: email) do
      {:ok, %{"result" => [record]}} ->
        record
        |> Term.parse_struct(__MODULE__)
        |> Term.tag(:ok)

      {:ok, %{"result" => []}} ->
        :not_found
    end
  end

  @doc """
  Insert a new user into the database.
  """
  @spec insert(map | keyword) :: {:ok, t} | Particle.invalid()
  def insert(params) do
    sql = """
    INSERT INTO User SET
    email = :email
    """

    with {:ok, data} <- Orientdb.command(sql, params) do
      Metrics.increment_counter("user/insert")

      data
      |> Map.fetch!("result")
      |> hd()
      |> Term.parse_struct(__MODULE__)
      |> Term.tag(:ok)
    end
  end

  @doc """
  Insert a new User into the database, fetching it there already is one
  with the given email.
  """
  @spec fetch_or_insert(map | keyword) :: {:ok, t} | Particle.invalid()
  def fetch_or_insert(params) do
    with :not_found <- fetch_by_email(params[:email]) do
      insert(params)
    end
  end
end
