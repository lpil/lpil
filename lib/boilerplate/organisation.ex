defmodule Boilerplate.Organisation do
  @moduledoc """
  An organisation that users and rotas can belong to.
  """

  use Ecto.Schema
  require Ecto.Query
  alias Boilerplate.{Repo, Membership}
  alias Ecto.Query

  @optional_params []
  @required_params [:name]

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  schema "organisations" do
    field(:name, :string)
    has_many(:memberships, Membership)
    timestamps(type: :utc_datetime)
  end

  @type t :: %__MODULE__{}

  @doc """
  Prepare and validate basic changes to a user.
  """
  def changeset(user, params \\ %{}) do
    import Ecto.Changeset

    user
    |> cast(params, @required_params)
    |> cast(params, @optional_params)
    |> validate_required(@required_params)
    |> validate_length(:name, min: 2, max: 50)
    |> unique_constraint(:name)
  end

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
  Insert a new Organisation into the database.
  """
  @spec insert(map | keyword) :: {:ok, t} | {:error, Ecto.Changeset.t()}
  def insert(params) do
    %__MODULE__{}
    |> changeset(params)
    |> Repo.insert()
  end

  @doc """
  Add a user to the organisation.
  """
  @spec insert_user(t, User.t()) :: {:ok, t} | {:error, Ecto.Changeset.t()}
  def insert_user(organisation, user) do
    Membership.insert(user, organisation, :user)
  end

  @doc """
  Add an admin to the organisation.
  """
  @spec insert_admin(t, User.t()) :: {:ok, t} | {:error, Ecto.Changeset.t()}
  def insert_admin(organisation, user) do
    Membership.insert(user, organisation, :admin)
  end
end
