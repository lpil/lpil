defmodule Fawkes.BlogControllerTest do
  use Fawkes.ConnCase

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
end
