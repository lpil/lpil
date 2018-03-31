defmodule FcatWeb.StatusControllerTest do
  use FcatWeb.ConnCase

  test "GET /", %{conn: conn} do
    conn = get(conn, "/v1/status")

    assert json_response(conn, 200) == %{"neo4j_connected" => true}
  end
end
