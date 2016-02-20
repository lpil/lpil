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

  def show(conn, %{"id" => slug}) do
    article =
      Article
      |> PublishableQuery.published
      |> Repo.get_by!(slug: slug)
    render conn, "show.html", article: article
  end
end
