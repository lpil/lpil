defmodule BlockParty.PageController do
  use BlockParty.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
