defmodule Boilerplate.PasswordResetTokenTest do
  use Boilerplate.DataCase, async: true
  alias Boilerplate.{User, PasswordResetToken}

  @user_params Fixture.user_params()

  test "creation, unexpired lookup" do
    {:ok, user} = User.insert(@user_params)

    assert :not_found = PasswordResetToken.fetch_for_user(user)

    assert {:ok, token} = PasswordResetToken.create_for_user(user)
    assert PasswordResetToken.fetch_for_user(user) == {:ok, token}
    assert PasswordResetToken.fetch(token.id) == {:ok, token}
    assert DateTime.diff(token.expires_at, DateTime.utc_now()) > 3590
    assert DateTime.diff(token.expires_at, DateTime.utc_now()) < 3610

    # Creation again removes the existing record
    assert {:ok, new_token} = PasswordResetToken.create_for_user(user)
    assert PasswordResetToken.fetch_for_user(user) == {:ok, new_token}
    assert PasswordResetToken.fetch(new_token.id) == {:ok, new_token}
    assert new_token.id != token.id
  end

  test "expired" do
    {:ok, user} = User.insert(@user_params)

    assert {:ok, token} =
             Repo.insert(%PasswordResetToken{
               expires_at: DateTime.utc_now() |> Timex.shift(seconds: -1),
               user_id: user.id
             })

    assert :expired = PasswordResetToken.fetch_for_user(user)
    assert :expired = PasswordResetToken.fetch(token.id)
  end
end
