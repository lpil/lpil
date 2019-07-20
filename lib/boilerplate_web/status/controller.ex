defmodule BoilerplateWeb.Status.Controller do
  use BoilerplateWeb, :controller

  def show(conn, _params) do
    postgres_connected = Boilerplate.Repo.ping?()
    ok = postgres_connected

    data = %{
      postgres_connected: postgres_connected
    }

    conn
    |> put_status(if ok, do: 200, else: 503)
    |> render("show.json", data: data)
  end
end
