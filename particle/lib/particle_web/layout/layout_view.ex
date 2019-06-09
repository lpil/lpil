defmodule ParticleWeb.LayoutView do
  use ParticleWeb, :view

  def drawer_link(opts, do: content) do
    method = opts[:method] || :get
    to = opts[:to] || raise("to must be passed")
    icon = opts[:icon] || raise("icon must be passed")
    icon_class = "mdl-color-text--blue-grey-400 material-icons"

    link(to: to, method: method, class: "mdl-navigation__link") do
      [
        content_tag(:i, class: icon_class, role: "presentation") do
          icon
        end,
        content
      ]
    end
  end
end
