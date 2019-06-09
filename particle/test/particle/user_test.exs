defmodule Particle.UserTest do
  use Particle.DataCase, async: true
  alias Particle.User

  @params Fixture.user_params()

  describe "insert/1" do
    test "new" do
      assert {:ok, %Particle.User{} = user} = User.insert(@params)
      assert user.id =~ ~r"........-....-....-....-............"
      assert user.email == "e@ma.il"
      assert user.inserted_at
      assert user.updated_at
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
      assert user.email == "e@ma.il"
    end

    test "pre-existing" do
      assert {:ok, user} = User.fetch_or_insert(@params)
      assert User.fetch_or_insert(@params) == {:ok, user}
    end
  end

  describe "fetch/1" do
    test "not found" do
      uuid = Ecto.UUID.generate()
      assert User.fetch(uuid) == :not_found
    end

    test "found" do
      assert {:ok, %{id: id}} = User.insert(%{email: "e@ma.il"})
      assert {:ok, user} = User.fetch(id)
      assert %User{email: "e@ma.il"} = user
    end
  end

  describe "fetch_by_email/1" do
    test "not found" do
      assert User.fetch_by_email("nope") == :not_found
    end

    test "found" do
      email = "e@ma.il"
      assert {:ok, _} = User.insert(%{email: email})
      assert {:ok, user} = User.fetch_by_email(email)
      assert user.email == email
    end
  end
end
