defmodule Fawkes.ArticleControllerTest do
  use Fawkes.ConnCase

  setup %{conn: conn} = config do
    attrs = config[:login_as]
    if attrs do
      user = insert_user(attrs)
      conn = sign_in(conn, user)
      {:ok, conn: conn, user: user}
    else
      :ok
    end
  end


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
end
