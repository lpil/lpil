defmodule FcatWeb.StatusController do
  use FcatWeb, :controller

  def show(conn, _params) do
    neo4j_connected = Fcat.Neo4j.ping?()
    ok = neo4j_connected

    data = %{
      neo4j_connected: neo4j_connected
    }

    conn
    |> put_status(if ok, do: 200, else: 503)
    |> render("show.json", data: data)
  end
end
