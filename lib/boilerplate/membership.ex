defmodule Boilerplate.Membership do
  @moduledoc """
  A relationship between a User and an Organisation.
  """

  use Ecto.Schema
  require Ecto.Query
  alias Boilerplate.{Repo, Organisation, User}

  @type membership_type() :: :admin | :user
  require EctoEnum
  EctoEnum.defenum(TypeEnum, :membership_type, [:admin, :user])

  schema "memberships" do
    field(:type, TypeEnum)
    belongs_to(:user, User, type: :binary_id)
    belongs_to(:organisation, Organisation, type: :binary_id)
    timestamps(type: :utc_datetime)
  end

  @type t :: %__MODULE__{}

  @spec insert(User.t(), Organisation.t(), membership_type()) ::
          {:ok, t} | {:error, Ecto.Changeset.t()}
  def insert(user, organisation, type) do
    import Ecto.Changeset

    %__MODULE__{}
    |> cast(%{}, [])
    |> put_change(:type, type)
    |> put_assoc(:user, user)
    |> put_assoc(:organisation, organisation)
    |> validate_required([:user, :organisation])
    |> unique_constraint(
      :user,
      name: :memberships_user_id_organisation_id_index,
      message: "is already a member"
    )
    |> Repo.insert()
  end
end
