defmodule ParticleWeb.StatusView do
  use ParticleWeb, :view

  def render("show.json", %{data: data}) do
    data
  end
end
