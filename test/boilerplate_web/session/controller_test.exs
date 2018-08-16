defmodule BoilerplateWeb.Session.ControllerTest do
  use BoilerplateWeb.ConnCase
  alias Boilerplate.User

  describe "GET /login" do
    test "200", %{conn: conn} do
      conn = get(conn, "/login")
      assert body = html_response(conn, 200)
      assert body =~ "Login"
    end
  end

  describe "POST /login" do
    @params Fixture.user_params()

    test "new user", %{conn: conn} do
      conn = post(conn, "/login", %{creds: @params})

      assert redirected_to(conn) == "/register?" <> Plug.Conn.Query.encode(email: @params.email)
    end

    test "no email", %{conn: conn} do
      {:ok, _} = User.insert(@params)
      conn = post(conn, "/login", %{"creds" => Map.put(@params, :email, "")})
      assert body = html_response(conn, 422)
      assert body =~ "must be present"
    end

    test "exiting user, incorrect password", %{conn: conn} do
      {:ok, _} = User.insert(@params)
      conn = post(conn, "/login", %{"creds" => Map.put(@params, :password, "I dunno tbh")})
      assert body = html_response(conn, 422)
      assert body =~ "is not correct"
    end

    test "exiting user, correct password", %{conn: conn} do
      {:ok, user} = User.insert(@params)
      conn = post(conn, "/login", %{"creds" => @params})
      assert redirected_to(conn) == "/dashboard"
      assert Plug.Conn.get_session(conn, :uid) == user.id
    end
  end
end
