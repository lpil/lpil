defmodule Boilerplate.UserTest do
  use Boilerplate.DataCase, async: true
  alias Boilerplate.{User, Organisation}

  @params Fixture.user_params()

  describe "insert/1" do
    test "new" do
      assert {:ok, %Boilerplate.User{} = user} = User.insert(@params)
      assert user.id =~ ~r"........-....-....-....-............"
      assert user.email == @params.email
      assert user.inserted_at
      assert user.updated_at
    end

    test "invalid - no name" do
      assert {:error, changeset} = User.insert(Map.delete(@params, :name))
      assert errors_on(changeset) == %{name: ["can't be blank"]}
    end

    test "invalid - no email" do
      assert {:error, changeset} = User.insert(Map.delete(@params, :email))
      assert errors_on(changeset) == %{email: ["can't be blank"]}
    end

    test "invalid - incorrect email format" do
      assert {:error, changeset} = User.insert(Map.put(@params, :email, "1"))
      assert errors_on(changeset) == %{email: ["has invalid format"]}
    end

    test "pre-existing" do
      assert {:ok, _user} = User.insert(@params)
      assert {:error, changeset} = User.insert(@params)
      assert errors_on(changeset) == %{email: ["has already been taken"]}
    end
  end

  describe "fetch_or_insert/1" do
    test "new" do
      assert {:ok, user} = User.fetch_or_insert(@params)
      assert user.email == @params.email
    end

    test "pre-existing" do
      assert {:ok, user} = User.fetch_or_insert(@params)
      assert User.fetch_or_insert(@params) == {:ok, %{user | password: nil}}
    end
  end

  describe "fetch/1" do
    test "not found" do
      uuid = Ecto.UUID.generate()
      assert User.fetch(uuid) == :not_found
    end

    test "found" do
      {:ok, user} = User.insert(@params)
      {:ok, organisation} = Fixture.organisation_params() |> Organisation.insert()
      {:ok, membership} = Organisation.insert_user(organisation, user)
      assert {:ok, persisted_user} = User.fetch(user.id)
      assert persisted_user.email == @params.email
      assert [persisted_membership] = persisted_user.memberships
      assert persisted_membership.id == membership.id
      assert persisted_membership.organisation.id == organisation.id
    end
  end

  describe "fetch_by_email/1" do
    test "not found" do
      assert User.fetch_by_email("nope") == :not_found
    end

    test "found" do
      assert {:ok, _} = User.insert(@params)
      assert {:ok, user} = User.fetch_by_email(@params.email)
      assert user.email == @params.email
    end
  end

  test "confirm_email/1" do
    {:ok, user} = @params |> Map.delete(:email_confirmed_at) |> User.insert()
    refute user.email_confirmed_at
    assert :ok = User.confirm_email(user)
    {:ok, new_user} = User.fetch(user.id)
    assert new_user.email_confirmed_at
  end
end
