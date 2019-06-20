defmodule Fawkes.BlogView do
  @moduledoc false
  use Fawkes.Web, :view

  defdelegate body_content(article), to: Fawkes.ArticleView
end
