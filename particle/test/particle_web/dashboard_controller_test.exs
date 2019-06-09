defmodule ParticleWeb.DashboardControllerTest do
  use ParticleWeb.ConnCase

  test "GET /dashboard when not logged in", %{conn: conn} do
    conn = get(conn, "/dashboard")
    assert redirected_to(conn) == "/"
    assert get_flash(conn) == %{"error" => "Please login to continue"}
  end

  test "GET /dashboard", %{conn: conn} do
    {:ok, user} = Fixture.user()
    conn = conn |> put_session(:uid, user.id) |> get("/dashboard")
    assert html_response(conn, 200) =~ "Particle"
  end
end
