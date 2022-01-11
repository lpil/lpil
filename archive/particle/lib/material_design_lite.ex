defmodule MaterialDesignLite do
  @moduledoc """
  View helpers for working with the MaterialDesignLite CSS framework.
  """

  import Phoenix.HTML.Tag, only: [content_tag: 3]

  @doc """
  Wrap some content in an MDL card.

      <%= card columns: 6 do %>
        <h1> Hello, world! </h1>
      <% end %>

  ## Options

  - :columns - Number of columns to use in the grid. Defaults to 12.
  - :class - Any extra classes to add to the card.

  ## Examples

  No options:

      iex> Phoenix.HTML.safe_to_string card(do: "Content")
      ~S(<div class="mdl-color--white mdl-shadow--2dp mdl-cell mdl-cell--12-col mdl-grid">Content</div>)

  Custom class:

      iex> Phoenix.HTML.safe_to_string card([class: "hello"], do: "Content")
      ~S(<div class="mdl-color--white mdl-shadow--2dp mdl-cell mdl-cell--12-col mdl-grid hello">Content</div>)

  Different number of columns:

      iex> Phoenix.HTML.safe_to_string card([columns: 6], do: "Content")
      ~S(<div class="mdl-color--white mdl-shadow--2dp mdl-cell mdl-cell--6-col mdl-grid">Content</div>)
  """
  def card(opts \\ [], do: content) do
    columns = opts[:columns] || 12

    extra_classes =
      case opts[:class] do
        nil -> []
        c -> [?\s, c]
      end

    class = [
      "mdl-color--white mdl-shadow--2dp mdl-cell",
      [" mdl-cell--", to_string(columns), "-col"],
      " mdl-grid",
      extra_classes
    ]

    content_tag(:div, content, class: class)
  end
end
