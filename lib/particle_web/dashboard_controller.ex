defmodule ParticleWeb.DashboardController do
  use ParticleWeb, :controller

  def show(conn, _params) do
    render(conn, "show.html")
  end
end
