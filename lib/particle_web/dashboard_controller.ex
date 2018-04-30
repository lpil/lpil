defmodule ParticleWeb.DashboardController do
  use ParticleWeb, :controller

  def show(conn, _params) do
    conn
    |> render("show.html")
  end
end
