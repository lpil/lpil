defmodule BoilerplateWeb.EmailConfirmation.ControllerTest do
  use BoilerplateWeb.ConnCase
  alias Boilerplate.User
  import Swoosh.TestAssertions
  alias Boilerplate.EmailConfirmationToken

  describe "GET /email-confirmation" do
    test "not logged in", ctx do
      conn = get(ctx.conn, "/email-confirmation")
      assert redirected_to(conn, 302) == "/login"
    end

    test "logged in", ctx do
      {:ok, user} = Fixture.user_params() |> User.insert()

      conn =
        ctx.conn
        |> put_session(:uid, user.id)
        |> get("/email-confirmation")

      assert body = html_response(conn, 200)
      assert body =~ "Please click the confirmation link"
    end
  end

  describe "POST /email-confirmation" do
    test "not logged in", ctx do
      conn = post(ctx.conn, "/email-confirmation")
      assert redirected_to(conn, 302) == "/login"
    end

    test "logged in", ctx do
      {:ok, user} = Fixture.user_params() |> User.insert()

      conn =
        ctx.conn
        |> put_session(:uid, user.id)
        |> post("/email-confirmation")

      assert body = html_response(conn, 200)
      assert body =~ "Please check your inbox"
      assert_email_sent(subject: "Welcome to Boilerplate! Please confirm your email")
    end
  end

  describe "DELETE /email-confirmation/:id" do
    test "not logged in", ctx do
      conn = get(ctx.conn, "/email-confirmation/some-uuid")
      assert redirected_to(conn, 302) == "/login"
    end

    test "logged in, no token", ctx do
      {:ok, user} = Fixture.user_params(email_confirmed_at: nil) |> User.insert()

      conn =
        ctx.conn
        |> put_session(:uid, user.id)
        |> get("/email-confirmation/some-uuid")

      assert html_response(conn, 404)
    end

    test "logged in, matching token", ctx do
      {:ok, user} = Fixture.user_params(email_confirmed_at: nil) |> User.insert()
      {:ok, token} = EmailConfirmationToken.for_user(user)
      refute user.email_confirmed_at

      conn =
        ctx.conn
        |> put_session(:uid, user.id)
        |> get("/email-confirmation/" <> token.id)

      assert redirected_to(conn, 302) =~ "/dashboard"
      assert get_flash(conn) == %{"info" => "Thanks! Your email has been confirmed"}
      {:ok, user} = User.fetch(user.id)
      assert user.email_confirmed_at
    end
  end
end
