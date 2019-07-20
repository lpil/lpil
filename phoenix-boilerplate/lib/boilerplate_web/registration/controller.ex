defmodule BoilerplateWeb.Registration.Controller do
  @moduledoc """
  Allows a new user to sign up.
  """

  use BoilerplateWeb, :controller
  alias Boilerplate.{User, EmailConfirmationToken}
  alias BoilerplateWeb.Registration
  require Logger

  def show(conn, params) do
    changeset = User.registration_changeset(%User{}, %{email: params["email"]})

    conn
    |> render("new.html", changeset: changeset)
  end

  def create(conn, params) do
    case params |> Map.get("user", %{}) |> User.register() do
      {:error, changeset} ->
        create_fail(conn, changeset)

      {:ok, user} ->
        create_success(conn, user)
    end
  end

  defp create_fail(conn, changeset) do
    conn
    |> put_status(422)
    |> render("new.html", changeset: changeset)
  end

  defp create_success(conn, user) do
    {:ok, token} = EmailConfirmationToken.for_user(user)

    {:ok, _} =
      user
      |> Registration.Email.confirmation_email(token)
      |> BoilerplateWeb.Mailer.deliver()

    Logger.info(
      "user #{user.id} registered",
      action: :user_registered,
      user_id: user.id
    )

    conn
    |> put_session(:uid, user.id)
    |> redirect(to: email_confirmation_path(conn, :index))
  end
end
