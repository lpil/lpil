defmodule FcatWeb.AuthControllerTest do
  use FcatWeb.ConnCase

  test "GET /login/unknown-provider", %{conn: conn} do
    assert_raise Phoenix.Router.NoRouteError, fn -> get(conn, "/login/unknown-provider") end
  end

  test "GET /login/auth0", %{conn: conn} do
    conn = get(conn, "/login/auth0")

    assert redirected_to(conn) ==
             "https://example.auth0.com/authorize?client_id=client_id&redirect_uri=http%3A%2F%2Fwww.example.com%2Flogin%2Fauth0%2Fcallback&response_type=code&scope=openid+profile+email"
  end

  # How do we test this? It will hit auth0. Hum.
  test "GET /login/auth0/callback"
end
