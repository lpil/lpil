defmodule FcatWeb.PageController do
  use FcatWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
