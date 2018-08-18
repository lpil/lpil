defmodule BoilerplateWeb.Registration.EmailTest do
  use ExUnit.Case, async: true
  alias Boilerplate.{User, EmailConfirmationToken}
  alias BoilerplateWeb.Registration

  test "confirmation_email/2" do
    {:ok, user} = Fixture.user_params() |> User.insert()
    {:ok, token} = EmailConfirmationToken.for_user(user)
    email = Registration.Email.confirmation_email(user, token)
    assert email.subject =~ "Please confirm your email"
    assert email.html_body =~ "localhost:4001/email-confirmation/" <> token.id
  end
end
