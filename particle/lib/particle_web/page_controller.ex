defmodule ParticleWeb.PageController do
  use ParticleWeb, :controller

  def index(conn, _params) do
    conn
    |> put_layout("unauthenticated.html")
    |> render("index.html")
  end
end
