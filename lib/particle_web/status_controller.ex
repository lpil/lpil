defmodule ParticleWeb.StatusController do
  use ParticleWeb, :controller

  def show(conn, _params) do
    orientdb_connected = Orientdb.ping()
    ok = orientdb_connected

    data = %{
      orientdb_connected: orientdb_connected
    }

    conn
    |> put_status(if ok, do: 200, else: 503)
    |> render("show.json", data: data)
  end
end
