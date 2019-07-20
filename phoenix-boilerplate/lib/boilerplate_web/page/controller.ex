defmodule BoilerplateWeb.Page.Controller do
  use BoilerplateWeb, :controller

  def index(conn, _params) do
    conn
    |> render("index.html")
  end
end
