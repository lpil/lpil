defmodule EmailConfirmationTokenTest do
  use Boilerplate.DataCase, async: true
  alias Boilerplate.{User, EmailConfirmationToken}

  @user_params Fixture.user_params()

  test "for_user/1" do
    {:ok, user} = User.insert(@user_params)
    assert {:ok, token} = EmailConfirmationToken.for_user(user)
    assert EmailConfirmationToken.for_user(user) == {:ok, token}
  end

  test "delete/1" do
    {:ok, user} = User.insert(@user_params)
    assert {:ok, token} = EmailConfirmationToken.for_user(user)
    assert :ok = EmailConfirmationToken.delete(token)
    assert {:ok, new_token} = EmailConfirmationToken.for_user(user)
    assert token.id != new_token.id
  end
end
