defmodule BoilerplateWeb.EmailConfirmation.Controller do
  @moduledoc """
  Allows a user to confirm their email, or to request a new confirmation
  email to be sent.
  """

  use BoilerplateWeb, :controller
  alias BoilerplateWeb.{Registration, Mailer, Error}
  alias Boilerplate.{User, EmailConfirmationToken}

  def index(conn, _params) do
    conn
    |> render("index.html")
  end

  def create(conn, _params) do
    user = conn.assigns.user
    {:ok, token} = Boilerplate.EmailConfirmationToken.for_user(user)

    {:ok, _} =
      user
      |> Registration.Email.confirmation_email(token)
      |> Mailer.deliver()

    flash = gettext("A new email has been sent. Please check your inbox in a few minutes")

    conn
    |> put_flash(:info, flash)
    |> render("index.html")
  end

  def show(conn, params) do
    user = conn.assigns.user
    token_id = params["id"]
    {:ok, token} = EmailConfirmationToken.for_user(user)

    case token.id do
      ^token_id ->
        :ok = EmailConfirmationToken.delete(token)
        :ok = User.confirm_email(user)

        conn
        |> put_flash(:info, gettext("Thanks! Your email has been confirmed"))
        |> redirect(to: dashboard_path(conn, :show))

      _ ->
        conn
        |> put_status(404)
        |> render(Error.View, "404.html")
    end
  end
end
