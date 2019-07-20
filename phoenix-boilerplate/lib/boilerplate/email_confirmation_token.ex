defmodule Boilerplate.EmailConfirmationToken do
  use Ecto.Schema
  alias Boilerplate.{Repo, User}

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  schema "email_confirmation_tokens" do
    belongs_to(:user, User, type: :binary_id)
    timestamps(type: :utc_datetime)
  end

  @type t :: %__MODULE__{}

  @spec for_user(User.t()) :: {:ok, t}
  def for_user(user = %User{}) do
    require Ecto.Query
    alias Ecto.Query

    case __MODULE__ |> Query.where(user_id: ^user.id) |> Repo.fetch() do
      {:ok, _token} = result ->
        result

      :not_found ->
        Repo.insert(%__MODULE__{user_id: user.id})
    end
  end

  @spec delete(t()) :: :ok
  def delete(token) do
    {:ok, _} = Repo.delete(token)
    :ok
  end
end
