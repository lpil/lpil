defmodule ParticleWeb.AuthControllerTest do
  use ParticleWeb.ConnCase, async: false
  alias Particle.User

  test "GET /login/unknown-provider", %{conn: conn} do
    assert_raise Phoenix.Router.NoRouteError, fn -> get(conn, "/login/unknown-provider") end
  end

  test "GET /login/auth0", %{conn: conn} do
    conn = get(conn, "/login/auth0")

    assert redirected_to(conn) ==
             "https://example.auth0.com/authorize?client_id=client_id&redirect_uri=http%3A%2F%2Fwww.example.com%2Flogin%2Fauth0%2Fcallback&response_type=code&scope=openid+profile+email"
  end

  test "DELETE /session", ctx do
    conn = ctx.conn |> put_session(:uid, "1") |> delete("/session")
    assert get_session(conn, :uid) == nil
    assert redirected_to(conn, 302) == "/"
  end

  # TODO: Golden path selenium style integration test for login.
  # We can't meaningfully test the auth0 oauth interaction at this level.

  describe "callback/2" do
    test "auth success", %{conn: conn} do
      auth = %Ueberauth.Auth{uid: "123456", info: %{email: "some@email.net"}}

      conn =
        conn
        |> Plug.Conn.assign(:ueberauth_auth, auth)
        |> ParticleWeb.AuthController.callback([])

      assert redirected_to(conn, 302) == "/dashboard"
      assert {:ok, _user} = User.fetch_by_email("some@email.net")
    end
  end
end
