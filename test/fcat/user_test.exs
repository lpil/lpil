defmodule Fcat.UserTest do
  use ExUnit.Case, async: false
  import TestHelper
  alias Fcat.User

  setup [:truncate_database]

  @params %User.Insert{email: "e@ma.il", id: "123"}

  describe "insert/1" do
    test "new" do
      assert {:ok, user} = User.insert(@params)
      assert %Fcat.User{email: "e@ma.il", id: "123", inserted_at: inserted_at} = user
      assert is_integer(inserted_at)
    end

    @tag :skip
    test "invalid - no id" do
      assert {:error, _} = User.insert(Map.put(@params, :id, nil))
    end

    @tag :skip
    test "invalid - no email" do
      assert {:error, _} = User.insert(Map.put(@params, :email, nil))
    end

    test "pre-existing" do
      assert {:ok, _user} = User.insert(@params)
      assert {:error, [code: code, message: message]} = User.insert(@params)
      assert code == "Neo.ClientError.Schema.ConstraintValidationFailed"
      assert message =~ "already exists with label `User` and property `id` = '123'"
    end
  end

  describe "fetch_or_insert/1" do
    test "new" do
      assert {:ok, user} = User.fetch_or_insert(@params)
      assert %Fcat.User{email: "e@ma.il", id: "123", inserted_at: inserted_at} = user
      assert is_integer(inserted_at)
    end

    test "pre-existing" do
      assert {:ok, user} = User.fetch_or_insert(@params)
      params = Map.put(@params, :email, "")
      assert User.fetch_or_insert(params) == {:ok, user}
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
