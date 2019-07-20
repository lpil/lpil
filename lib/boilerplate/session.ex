defmodule Boilerplate.Session do
  @moduledoc """
  The creation of sessions using credentials. i.e. when the user logs in
  with their username and password via the login page.
  """

  import Boilerplate.Gettext

  defmodule Creds do
    @moduledoc """
    Struct for storing errors relating to session credentials.
    """
    use Ecto.Schema

    @primary_key false
    embedded_schema do
      field(:email, :string)
      field(:password, :string)
    end

    @type t :: %__MODULE__{}

    def changeset(creds \\ %__MODULE__{}, params \\ %{}) do
      creds
      |> Ecto.Changeset.cast(params, [:email, :password])
    end
  end

  @doc """
  Create a session from usename and password. Returns a user if successful,
  a changeset if not.

  The mechanism for persisting the session is left up to the caller as it
  depends on the interface the user is using (API, HTML, CLI, etc).

  """
  def create_session(params) do
    email = get_in(params, ["creds", "email"])
    password = get_in(params, ["creds", "password"])

    case Boilerplate.User.fetch_for_credentials(email, password) do
      :not_found ->
        {:not_found, email}

      :incorrect_password ->
        changeset_with_error(:password, gettext("is not correct"))
        |> Term.tag(:error)

      :email_required ->
        changeset_with_error(:email, gettext("must be present"))
        |> Term.tag(:error)

      {:ok, _user} = result ->
        result
    end
  end

  defp changeset_with_error(field, error) do
    Creds.changeset()
    |> Ecto.Changeset.add_error(field, error)
    |> Map.put(:action, :insert)
  end
end
