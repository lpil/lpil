defmodule Fawkes.SessionController do
  @moduledoc """
  Responsible for signing in and out of users.
  """
  use Fawkes.Web, :controller

  alias Comeonin.Bcrypt
  alias Fawkes.Repo

  def new(conn, _params) do
    render conn, "new.html"
  end

  def create(conn, %{"session" => %{"username" => u, "password" => p}}) do
    conn
    |> try_login(u, p, repo: Repo)
    |> case do
      {:ok, conn} ->
        conn
        |> put_flash(:info, "Welcome back")
        |> redirect(to: page_path(conn, :index))
      {:error, _reason, conn} ->
        conn
        |> put_flash(:error, "Invalid username/password combination")
        |> render("new.html")
    end
  end

  def delete(conn, _params) do
    conn
    |> Guardian.Plug.sign_out
    |> put_flash(:info, "Signed out successfully")
    |> redirect(to: "/")
  end


  defp try_login(conn, username, password, opts) do
    repo = Keyword.fetch!(opts, :repo)
    user = repo.get_by(Fawkes.User, username: username)
    cond do
      user && Bcrypt.checkpw(password, user.password_hash) ->
        {:ok, Guardian.Plug.sign_in(conn, user)}
      user ->
        {:error, :unauthorized, conn}
      true ->
        Bcrypt.dummy_checkpw()
        {:error, :not_found, conn}
    end
  end
end
