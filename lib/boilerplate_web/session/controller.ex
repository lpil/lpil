defmodule BoilerplateWeb.Session.Controller do
  @moduledoc """
  Handles the creation and destruction of user sessions for HTML
  pages. Creates a new user record if one is not already present.
  """

  use BoilerplateWeb, :controller
  alias Boilerplate.Session

  def new(conn, _params) do
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
    end
  end
end
