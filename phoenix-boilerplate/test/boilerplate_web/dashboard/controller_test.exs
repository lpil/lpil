defmodule BoilerplateWeb.Dashboard.ControllerTest do
  use BoilerplateWeb.ConnCase

  test "GET /dashboard when not logged in", %{conn: conn} do
    conn = get(conn, "/dashboard")
    assert redirected_to(conn) == "/login"
    assert get_flash(conn) == %{"error" => "Please login to continue"}
  end

  test "GET /dashboard when logged in but not confirmed", %{conn: conn} do
    {:ok, user} =
      Fixture.user_params() |> Map.delete(:email_confirmed_at) |> Boilerplate.User.insert()

    conn = conn |> put_session(:uid, user.id) |> get("/dashboard")
    assert redirected_to(conn) == "/email-confirmation"
    assert get_flash(conn) == %{"error" => "Please confirm your email to continue"}
  end

  test "GET /dashboard", %{conn: conn} do
    {:ok, user} = Fixture.user_params() |> Boilerplate.User.insert()
    conn = conn |> put_session(:uid, user.id) |> get("/dashboard")
    assert html_response(conn, 200) =~ "Welcome to the dashboard"
  end
end
