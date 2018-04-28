defmodule Particle.UserTest do
  use ExUnit.Case, async: false
  import TestHelper
  alias Particle.User

  setup [:truncate_database]

  @params Fixture.user_params()

  describe "insert/1" do
    test "new" do
      assert {:ok, %Particle.User{} = user} = User.insert(@params)
      assert user.email == "e@ma.il"
      assert user.id =~ ~r"........-....-....-....-............"
      assert user.inserted_at =~ ~r"\d\d\d\d-\d\d-\d\d \d\d:\d\d:\d\d"
    end

    test "invalid - no email" do
      assert {:invalid, errors} = User.insert(Map.delete(@params, :email))

      assert errors == [
               {:error, :email, :presence, "must be present"}
             ]
    end

    test "invalid - incorrect email format" do
      assert {:invalid, errors} = User.insert(Map.put(@params, :email, "1"))

      assert errors == [
               {:error, :email, :format, "must have the correct format"}
             ]
    end

    test "pre-existing" do
      assert {:ok, _user} = User.insert(@params)
      assert {:invalid, errors} = User.insert(@params)
      assert errors == [{:error, :email, :uniqueness, "has already been taken"}]
    end
  end

  describe "fetch_or_insert/1" do
    test "new" do
      assert {:ok, user} = User.fetch_or_insert(@params)
      assert user.email == "e@ma.il"
    end

    test "pre-existing" do
      assert {:ok, user} = User.fetch_or_insert(@params)
      assert User.fetch_or_insert(@params) == {:ok, user}
    end
  end

  describe "fetch/1" do
    test "not found" do
      assert User.fetch("unknown") == :not_found
    end

    test "found" do
      assert {:ok, %{id: id}} = User.insert(%{email: "e@ma.il"})
      assert {:ok, user} = User.fetch(id)
      assert %User{email: "e@ma.il"} = user
    end
  end
end
