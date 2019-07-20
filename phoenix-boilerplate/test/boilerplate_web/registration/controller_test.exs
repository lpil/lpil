defmodule BoilerplateWeb.Registration.ControllerTest do
  use BoilerplateWeb.ConnCase
  alias Boilerplate.User
  import Swoosh.TestAssertions

  describe "GET /register" do
    test "200", %{conn: conn} do
      conn = get(conn, "/register")
      assert body = html_response(conn, 200)
      assert body =~ "Register"
    end

    test "200, prepopulating email", %{conn: conn} do
      conn = get(conn, "/register?" <> Plug.Conn.Query.encode(email: "prepopulated@email"))
      assert body = html_response(conn, 200)
      assert body =~ "Register"
      assert body =~ "prepopulated@email"
    end
  end

  describe "POST /register" do
    @params Fixture.user_params()

    test "success", %{conn: conn} do
      conn = post(conn, "/register", %{user: @params})
      assert redirected_to(conn, 302) == "/email-confirmation"
      assert {:ok, user} = User.fetch_by_email(@params.email)
      assert Plug.Conn.get_session(conn, :uid) == user.id
      assert user.email_confirmed_at == nil
      assert_email_sent(subject: "Welcome to Boilerplate! Please confirm your email")
    end

    test "exiting user", %{conn: conn} do
      {:ok, _} = User.insert(@params)
      conn = post(conn, "/register", %{user: @params})
      assert body = html_response(conn, 422)
      assert body =~ "has already been taken"
    end
  end
end
