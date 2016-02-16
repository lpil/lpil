defmodule Fawkes.ArticleView do
  @moduledoc false
  use Fawkes.Web, :view

  alias Fawkes.Article

  defp body_content(%Article{ body: body }) when is_binary(body) do
    body
    |> HtmlSanitizeEx.basic_html
    |> raw
  end
end
