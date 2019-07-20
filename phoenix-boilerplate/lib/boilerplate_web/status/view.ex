defmodule BoilerplateWeb.Status.View do
  use BoilerplateWeb, :view

  def render("show.json", %{data: data}) do
    data
  end
end
