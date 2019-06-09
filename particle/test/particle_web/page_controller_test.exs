defmodule ParticleWeb.PageControllerTest do
  use ParticleWeb.ConnCase

  test "GET /", %{conn: conn} do
    conn = get(conn, "/")
    assert html_response(conn, 200) =~ "Particle"
  end
end
