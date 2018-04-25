defmodule ParticleWeb.StatusControllerTest do
  use ParticleWeb.ConnCase

  test "GET /", %{conn: conn} do
    conn = get(conn, "/v1/status")

    assert json_response(conn, 200) == %{"orientdb_connected" => true}
  end
end
