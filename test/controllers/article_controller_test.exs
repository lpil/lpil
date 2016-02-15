defmodule Fawkes.ArticleControllerTest do
  use Fawkes.ConnCase

  alias Fawkes.Article

  @attrs %{
    title: "Amazing Blog Post!",
    slug: "amazing-blog-post-0123456789",
    published_at: Ecto.DateTime.utc,
    body: """
    <p>This is our super amazing blog post.</p>
    <h1>Wow.</h1>
    """,
  }

  # new

  test "GET new when not signed in", %{conn: conn} do
    conn = get conn, article_path(conn, :new)
    assert redirected_to(conn) == "/"
  end

  @tag login_as: []
  test "GET new", %{conn: conn} do
    conn = get conn, article_path(conn, :new)
    assert html_response(conn, 200) =~ "New article"
  end


  # show

  test "GET show when not signed in", %{conn: conn} do
    article = Factory.create(:article)
    conn    = get conn, article_path(conn, :show, article)
    assert redirected_to(conn) == "/"
  end

  @tag login_as: []
  test "GET show", %{conn: conn} do
    article = Factory.create(:article)
    conn    = get conn, article_path(conn, :show, article)
    body    = html_response(conn, 200)
    assert body =~ article.title
    assert body =~ article.body
  end

  # create

  test "POST create when not signed in", %{conn: conn} do
    conn = post conn, article_path(conn, :create), article: @attrs
    assert redirected_to(conn) == "/"
    refute Repo.get_by(Article, @attrs)
  end

  @tag login_as: []
  test "POST create when successful", %{conn: conn} do
    conn = post conn, article_path(conn, :create), article: @attrs
    assert redirected_to(conn) == "/"
    assert Repo.get_by(Article, @attrs)
  end

  @tag login_as: []
  test "POST create with invalid attrs", %{conn: conn} do
    attrs = %{ @attrs | title: "" }
    conn = post conn, article_path(conn, :create), article: attrs
    body = html_response(conn, 200)
    assert body =~ "Please check the errors below"
    refute Repo.get_by(Article, @attrs)
  end
end
