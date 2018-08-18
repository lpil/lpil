defmodule BoilerplateWeb.Registration.Controller do
  @moduledoc """
  Allows a new user to sign up.
  """

  use BoilerplateWeb, :controller
  alias Boilerplate.{User, EmailConfirmationToken}
  alias BoilerplateWeb.Registration

  def new(conn, params) do
    changeset = User.registration_changeset(%User{}, %{email: params["email"]})

    conn
    |> render("new.html", changeset: changeset)
  end

  def create(conn, params) do
    case params |> Map.get("user", %{}) |> User.register() do
      {:error, changeset} ->
        conn
        |> put_status(422)
        |> render("new.html", changeset: changeset)

      {:ok, user} ->
        {:ok, token} = EmailConfirmationToken.for_user(user)

        {:ok, _} =
          user
          |> Registration.Email.confirmation_email(token)
          |> BoilerplateWeb.Mailer.deliver()

        conn
        |> put_session(:uid, user.id)
        |> redirect(to: email_confirmation_path(conn, :index))
    end
  end
end
