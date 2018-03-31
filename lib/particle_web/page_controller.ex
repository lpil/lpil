defmodule ParticleWeb.PageController do
  use ParticleWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
