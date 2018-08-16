defmodule BoilerplateWeb.Registration.Controller do
  @moduledoc """
  Allows a new user to sign up.
  """

  use BoilerplateWeb, :controller
  alias Boilerplate.User

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
        conn
        |> put_session(:uid, user.id)
        |> redirect(to: dashboard_path(conn, :show))
    end
  end
end
