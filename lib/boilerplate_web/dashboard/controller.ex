defmodule BoilerplateWeb.Dashboard.Controller do
  use BoilerplateWeb, :controller

  def show(conn, _params) do
    conn
    |> render("show.html")
  end
end
