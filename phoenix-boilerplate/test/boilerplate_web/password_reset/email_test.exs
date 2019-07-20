defmodule BoilerplateWeb.PasswordReset.EmailTest do
  use Boilerplate.DataCase, async: true
  alias Boilerplate.{User, PasswordResetToken}
  alias BoilerplateWeb.PasswordReset

  test "password_reset_email/2" do
    {:ok, user} = Fixture.user_params() |> User.insert()
    {:ok, token} = PasswordResetToken.create_for_user(user)
    email = PasswordReset.Email.password_reset_email(user, token)
    assert email.subject =~ "Reset your Boilerplate password"
    assert email.html_body =~ "localhost:4001/password-reset/" <> token.id
  end
end
