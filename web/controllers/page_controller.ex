defmodule Fawkes.PageController do
  @moduledoc """
  Responsible for serving of static pages.
  """
  use Fawkes.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
