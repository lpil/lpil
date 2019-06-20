defmodule Fawkes.User do
  @moduledoc """
  A person what can log in.
  """
  use Fawkes.Web, :model
  alias Comeonin.Bcrypt
  alias Ecto.Changeset

  schema "users" do
    field :username, :string
    field :email, :string
    field :password_hash, :string
    timestamps

    field :password,              :string, virtual: true
    field :password_confirmation, :string, virtual: true
  end

  @required_params ~w(email username)
  @optional_params ~w()

  @email_regex ~r/\A.+@.+\..+\z/


  @doc """
  Create a User changeset for the given params, and perform data validations.
  """
  def changeset(model, params \\ :empty) do
    model
    |> cast(params, @required_params, @optional_params)
    |> validate_length(:username, min: 3, max: 20)
    |> unique_constraint(:username)
    |> validate_format(:email, @email_regex)
    |> unique_constraint(:email)
  end


  @doc """
  Create a changeset for a new user insert
  """
  def registration_changeset(model, params) do
    model
    |> changeset(params)
    |> cast(params, ~w(password password_confirmation), [])
    |> validate_length(:password, min: 10, max: 128)
    |> validate_confirmation(:password)
    |> put_encrypted_password()
  end

  defp put_encrypted_password(changeset) do
    # Only hash password if the changeset is valid as hashing is expensive.
    case changeset do
      %Changeset{valid?: true, changes: %{password: pass}} ->
        put_change(changeset, :password_hash, Bcrypt.hashpwsalt(pass))
      _ ->
        changeset
    end
  end
end
