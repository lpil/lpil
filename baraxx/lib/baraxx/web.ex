defmodule Baraxx.Web do
  use Raxx.Server

  @impl Raxx.Server
  def handle_request(%{method: :GET, path: []}, _config) do
    response(:ok)
    |> set_header("content-type", "text/plain")
    |> set_body("Hello, World!")
  end

  def handle_request(%{method: :GET, path: ["hello", name]}, _config) do
    response(:ok)
    |> set_header("content-type", "text/plain")
    |> set_body("Hello, #{name}!")
  end

  def handle_request(_, _config) do
    response(:not_found)
    |> set_header("content-type", "text/plain")
    |> set_body("Nothing here :(")
  end
end
