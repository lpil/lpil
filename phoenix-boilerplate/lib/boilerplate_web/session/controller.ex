defmodule BoilerplateWeb.Session.Controller do
  @moduledoc """
  Handles the creation and destruction of user sessions for HTML
  pages.

  In the event that the account does not exist it redirects to
  the registration form.
  """

  use BoilerplateWeb, :controller
  alias Boilerplate.Session

  def show(conn, _params) do
    conn
    |> render("new.html", changeset: Session.Creds.changeset())
  end

  def create(conn, params) do
    case Session.create_session(params) do
      {:error, changeset} ->
        conn
        |> put_status(422)
        |> render("new.html", changeset: changeset)

      {:ok, user} ->
        conn
        |> put_session(:uid, user.id)
        |> redirect(to: dashboard_path(conn, :show))

      {:not_found, email} ->
        conn
        |> redirect(to: registration_path(conn, :show, %{email: email}))
    end
  end

  def delete(conn, _params) do
    conn
    |> delete_session(:uid)
    |> redirect(to: "/")
  end
end
