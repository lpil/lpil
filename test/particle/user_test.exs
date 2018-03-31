defmodule Particle.UserTest do
  use ExUnit.Case, async: false
  import TestHelper
  alias Particle.User

  setup [:truncate_database]

  @params %User.Insert{email: "e@ma.il", id: "123"}

  describe "insert/1" do
    test "new" do
      assert {:ok, user} = User.insert(@params)
      assert %Particle.User{email: "e@ma.il", id: "123", inserted_at: inserted_at} = user
      assert is_integer(inserted_at)
    end

    test "invalid - no id" do
      assert User.insert(Map.put(@params, :id, nil)) ==
               {:invalid, [{:error, :id, :presence, "must be present"}]}
    end

    test "invalid - no email" do
      assert {:invalid, errors} = User.insert(Map.put(@params, :email, nil))

      assert errors == [
               {:error, :email, :presence, "must be present"},
               {:error, :email, :format, "must have the correct format"}
             ]
    end

    test "pre-existing" do
      assert {:ok, _user} = User.insert(@params)
      assert {:invalid, errors} = User.insert(@params)
      assert errors == [{:error, :id, :uniqueness, "has already been taken"}]
    end
  end

  describe "fetch_or_insert/1" do
    test "new" do
      assert {:ok, user} = User.fetch_or_insert(@params)
      assert %Particle.User{email: "e@ma.il", id: "123", inserted_at: inserted_at} = user
      assert is_integer(inserted_at)
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
      assert {:ok, _} = User.fetch_or_insert(%User.Insert{email: "e@ma.il", id: "123"})
      assert {:ok, user} = User.fetch("123")
      assert %User{id: "123", email: "e@ma.il"} = user
    end
  end
end
