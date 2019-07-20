defmodule BoilerplateWeb.PasswordReset.ControllerTest do
  use BoilerplateWeb.ConnCase
  alias Boilerplate.{Repo, User, PasswordResetToken}
  import Swoosh.TestAssertions

  @user_params Fixture.user_params()

  describe "GET /password-reset/new" do
    test "success", %{conn: conn} do
      conn = get(conn, "/password-reset/new")
      assert body = html_response(conn, 200)
      assert body =~ "Reset your password"
    end
  end

  describe "POST /register" do
    test "success", %{conn: conn} do
      {:ok, user} = User.insert(@user_params)
      conn = post(conn, "/password-reset", %{email: user.email})
      assert redirected_to(conn, 302) == "/"

      assert get_flash(conn) == %{
               "info" => "A password reset link has been sent to your email address"
             }

      assert_email_sent(subject: "Reset your Boilerplate password")
    end
  end

  describe "GET /password-reset/:id" do
    test "not found", %{conn: conn} do
      conn = get(conn, "/password-reset/4d441856-c9bb-4fb9-bfcd-6d7f72252db5")
      assert html_response(conn, 404)
    end

    test "expired", %{conn: conn} do
      {:ok, user} = User.insert(@user_params)

      assert {:ok, token} =
               Repo.insert(%PasswordResetToken{
                 expires_at: DateTime.utc_now() |> Timex.shift(seconds: -1),
                 user_id: user.id
               })

      conn = get(conn, "/password-reset/" <> token.id)
      assert redirected_to(conn, 302) == "/password-reset/new"

      assert get_flash(conn) == %{
               "error" => "That password reset link has expired. Please try again"
             }
    end

    test "success", %{conn: conn} do
      {:ok, user} = User.insert(@user_params)
      {:ok, token} = PasswordResetToken.create_for_user(user)
      conn = get(conn, "/password-reset/" <> token.id)
      assert body = html_response(conn, 200)
      assert body =~ "Reset your password"
    end
  end

  describe "PATCH /password-reset/:id" do
    test "not found", %{conn: conn} do
      conn =
        patch(conn, "/password-reset/4d441856-c9bb-4fb9-bfcd-6d7f72252db5", %{
          user: %{password: "some-new-password"}
        })

      assert html_response(conn, 404)
    end

    test "expired", %{conn: conn} do
      {:ok, user} = User.insert(@user_params)

      assert {:ok, token} =
               Repo.insert(%PasswordResetToken{
                 expires_at: DateTime.utc_now() |> Timex.shift(seconds: -1),
                 user_id: user.id
               })

      conn =
        patch(conn, "/password-reset/" <> token.id, %{
          user: %{password: "some-new-password"}
        })

      assert redirected_to(conn, 302) == "/password-reset/new"

      assert get_flash(conn) == %{
               "error" => "That password reset link has expired. Please try again"
             }
    end

    test "password invalid", ctx do
      {:ok, user} = User.insert(@user_params)
      {:ok, token} = PasswordResetToken.create_for_user(user)

      conn =
        patch(ctx.conn, "/password-reset/" <> token.id, %{
          user: %{password: "pass123"}
        })

      assert body = html_response(conn, 422)
      refute body =~ "pass123"
      assert User.fetch_for_credentials(user.email, "pass123") == :incorrect_password
    end

    test "success", %{conn: conn} do
      {:ok, user} = User.insert(@user_params)
      {:ok, token} = PasswordResetToken.create_for_user(user)

      conn =
        patch(conn, "/password-reset/" <> token.id, %{
          user: %{password: "some-new-password"}
        })

      assert redirected_to(conn, 302) =~ "/dashboard"

      assert get_flash(conn) == %{
               "info" => "Your password has been updated"
             }

      assert {:ok, _user} = User.fetch_for_credentials(user.email, "some-new-password")
    end
  end
end
