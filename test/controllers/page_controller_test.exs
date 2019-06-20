defmodule BlockParty.PageControllerTest do
  use BlockParty.ConnCase

  test "GET /", %{conn: conn} do
    conn = get conn, "/"
    assert html_response(conn, 200) =~ "app.js"
  end
end
