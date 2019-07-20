defmodule BoilerplateWeb.PasswordReset.Controller do
  @moduledoc """
  Allows the user to reset their forgotten password via an email.
  """

  use BoilerplateWeb, :controller
  alias BoilerplateWeb.{PasswordReset, Error}
  alias Boilerplate.{User, PasswordResetToken}
  require Logger

  def new(conn, _params) do
    conn
    |> render("new.html")
  end

  def show(conn, params) do
    params["id"]
    |> PasswordResetToken.fetch()
    |> if_token_ok(conn, fn token ->
      render(
        conn,
        "show.html",
        id: token.id,
        changeset: User.registration_changeset(%User{})
      )
    end)
  end

  def update(conn, params) do
    params["id"]
    |> PasswordResetToken.fetch()
    |> if_token_ok(conn, fn token ->
      {:ok, user} = User.fetch(token.user_id)

      case User.update_password(user, get_in(params, ["user", "password"])) do
        {:ok, _} ->
          Logger.info(
            "user #{user.id} reset password",
            action: :user_reset_password,
            user_id: user.id
          )

          conn
          |> put_flash(:info, gettext("Your password has been updated"))
          |> redirect(to: dashboard_path(conn, :show))

        {:error, changeset} ->
          conn
          |> put_status(422)
          |> render("show.html", id: token.id, changeset: changeset)
      end
    end)
  end

  def create(conn, params) do
    case User.fetch_by_email(params["email"]) do
      :not_found ->
        Logger.info(
          "Password reset requested for unknown email",
          action: :password_reset_unknown
        )

      {:ok, user} ->
        send_password_reset(user)
    end

    conn
    |> put_flash(:info, gettext("A password reset link has been sent to your email address"))
    |> redirect(to: page_path(conn, :index))
  end

  defp send_password_reset(user) do
    {:ok, token} = PasswordResetToken.create_for_user(user)

    {:ok, _} =
      user
      |> PasswordReset.Email.password_reset_email(token)
      |> BoilerplateWeb.Mailer.deliver()

    Logger.info(
      "Password reset email sent",
      action: :password_reset_sent,
      user_id: user.id
    )
  end

  defp if_token_ok(token, conn, fun) do
    case token do
      {:ok, token} ->
        fun.(token)

      :expired ->
        conn
        |> put_flash(:error, gettext("That password reset link has expired. Please try again"))
        |> redirect(to: password_reset_path(conn, :new))

      :not_found ->
        conn
        |> put_status(404)
        |> render(Error.View, "404.html")
    end
  end
end
