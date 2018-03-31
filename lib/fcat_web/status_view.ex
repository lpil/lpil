defmodule FcatWeb.StatusView do
  use FcatWeb, :view

  def render("show.json", %{data: data}) do
    data
  end
end
