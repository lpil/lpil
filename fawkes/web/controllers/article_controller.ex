defmodule Fawkes.ArticleController do
  @moduledoc """
  Responsible for allowing admins to create and update blog articles.
  """
  use Fawkes.Web, :controller

  alias Fawkes.Article

  plug :scrub_params, "article" when action in [:create]


  def new(conn, _params) do
    changeset = Article.changeset(%Article{})
    render(conn, "new.html", changeset: changeset)
  end


  def show(conn, %{ "id" => slug}) do
    article = Repo.get_by!(Article, slug: slug)
    render(conn, "show.html", article: article)
  end


  def create(conn, %{"article" => params}) do
    changeset = Article.changeset(%Article{}, params)
    case Repo.insert(changeset) do
      {:ok, _post} ->
        conn
        |> put_flash(:info, "Article created successfully.")
        |> redirect(to: "/")
      {:error, changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end
end
