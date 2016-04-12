defmodule BlockParty.PageController do
  @moduledoc """
  Responsible for serving static pages. In the case, just our home page. :)
  """
  use BlockParty.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
