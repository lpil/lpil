defmodule Fawkes.BlogControllerTest do
  use Fawkes.ConnCase

  # GET index

  test """
  GET index displays articles list, but not those that are unpublished
  """, %{conn: conn} do
    future   = Ecto.DateTime.from_erl {{2030, 1, 1}, {1, 1, 1}}
    article1 = Factory.create(:article)
    article2 = Factory.create(:article)
    article3 = Factory.create(:article, published_at: future)
    conn = get conn, "/"
    body = html_response(conn, 200)
    assert body =~ article1.title
    assert body =~ article2.title
    refute body =~ article3.title
  end


  # GET show

  test """
  GET show displays a published article
  """, %{conn: conn} do
    article = Factory.create(:article)
    conn = get conn, blog_path(conn, :show, article)
    body = html_response(conn, 200)
    assert body =~ article.title
    assert body =~ String.replace(article.body, "\n", "")
  end

  test """
  GET show 404s for an unpublished article
  """, %{conn: conn} do
    future  = Ecto.DateTime.from_erl {{2030, 1, 1}, {1, 1, 1}}
    article = Factory.create(:article, published_at: future)
    assert_raise Ecto.NoResultsError, fn->
      get conn, blog_path(conn, :show, article)
    end
  end
end
