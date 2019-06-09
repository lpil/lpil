defmodule Particle.User do
  @moduledoc """
  A user of the system. How exotic!
  """

  use Ecto.Schema
  require Ecto.Query
  import Ecto.Changeset
  alias Particle.Repo
  alias Ecto.Query

  @optional_params []
  @required_params [:email]

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  schema "users" do
    field(:email, :string)
    timestamps(type: :utc_datetime)
  end

  @type t :: %__MODULE__{}

  @doc """
  Look up the user with the given id in the database.
  """
  @spec fetch(String.t()) :: {:ok, t} | :not_found
  def fetch(id) do
    __MODULE__
    |> Query.where(id: ^id)
    |> Repo.fetch()
  end

  @doc """
  Look up the user with the given email in the database.
  """
  @spec fetch_by_email(String.t()) :: {:ok, t} | :not_found
  def fetch_by_email(email) do
    __MODULE__
    |> Query.where(email: ^email)
    |> Repo.fetch()
  end

  @doc """
  Insert a new user into the database.
  """
  @spec insert(map | keyword) :: {:ok, t} | {:error, Ecto.Changeset.t()}
  def insert(params) do
    %__MODULE__{}
    |> changeset(params)
    |> Repo.insert()
  end

  @doc """
  Insert a new User into the database, fetching it there already is one
  with the given email.
  """
  @spec fetch_or_insert(map | keyword) :: {:ok, t} | {:error, Ecto.Changeset.t()}
  def fetch_or_insert(params) do
    with :not_found <- fetch_by_email(params[:email]) do
      insert(params)
    end
  end

  @doc false
  def changeset(user, params \\ %{}) do
    user
    |> cast(params, @required_params)
    |> cast(params, @optional_params)
    |> validate_required(@required_params)
    |> validate_format(:email, ~r/.@.+\../)
    |> unique_constraint(:email)
  end
end
