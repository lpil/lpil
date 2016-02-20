defmodule Fawkes.ErrorController do
  @moduledoc """
  Responsible handling error requests. Also serves as the handler for
  unauthenticated requsts.
  """
  use Fawkes.Web, :controller

  @doc """
  The unauthenticated function is called because this controller has been
  specified as the handler to Guardian.Plug.EnsureAuthenticated in the router.
  """
  def unauthenticated(conn, params) do
    conn
    |> put_status(404)
    |> put_view(Fawkes.ErrorView)
    |> render("404.html", [])
  end
end
