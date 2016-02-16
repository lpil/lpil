defmodule Fawkes.BlogController do
  @moduledoc """
  Responsible for serving of static pages.
  """
  use Fawkes.Web, :controller
  alias Fawkes.Article
  alias Fawkes.PublishableQuery

  def index(conn, _params) do
    articles = Article |> PublishableQuery.published |> Repo.all
    render conn, "index.html", articles: articles
  end
end
