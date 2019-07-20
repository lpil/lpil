defmodule Boilerplate.PasswordResetToken do
  use Ecto.Schema
  require Ecto.Query
  alias Ecto.Query
  alias Boilerplate.{Repo, User}

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  schema "password_reset_tokens" do
    belongs_to(:user, User, type: :binary_id)
    field(:expires_at, :utc_datetime)
    timestamps(type: :utc_datetime)
  end

  @type t :: %__MODULE__{}

  @spec create_for_user(t()) :: :not_found | {:ok, t()}
  def create_for_user(user) do
    __MODULE__ |> Query.where(user_id: ^user.id) |> Repo.delete_all()

    Repo.insert(%__MODULE__{
      expires_at: DateTime.utc_now() |> Timex.shift(hours: 1),
      user_id: user.id
    })
  end

  @spec fetch_for_user(t()) :: :not_found | :expired | {:ok, t()}
  def fetch_for_user(user) do
    __MODULE__
    |> Query.where(user_id: ^user.id)
    |> Repo.fetch()
    |> check_expiry()
  end

  @spec fetch(String.t()) :: :not_found | {:ok, t()}
  def fetch(id) do
    __MODULE__
    |> Query.where(id: ^id)
    |> Repo.fetch()
    |> check_expiry()
  end

  defp check_expiry(:not_found), do: :not_found

  defp check_expiry({:ok, token}) do
    if token.expires_at |> Timex.before?(DateTime.utc_now()) do
      :expired
    else
      {:ok, token}
    end
  end
end
