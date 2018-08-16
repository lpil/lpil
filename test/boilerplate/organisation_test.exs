defmodule Boilerplate.OrganisationTest do
  use Boilerplate.DataCase, async: true
  alias Boilerplate.{Organisation, User}

  @params Fixture.organisation_params()

  describe "insert/2" do
    test "new" do
      assert {:ok, %Organisation{} = org} = Organisation.insert(@params)
      assert org.id =~ ~r"........-....-....-....-............"
      assert org.name == @params.name
      assert org.inserted_at
      assert org.updated_at
    end

    test "invalid - no name" do
      assert {:error, changeset} = Organisation.insert(Map.delete(@params, :name))
      assert errors_on(changeset) == %{name: ["can't be blank"]}
    end

    test "pre-existing" do
      assert {:ok, _oeg} = Organisation.insert(@params)
      assert {:error, changeset} = Organisation.insert(@params)
      assert errors_on(changeset) == %{name: ["has already been taken"]}
    end
  end

  describe "fetch/1" do
    test "not found" do
      uuid = Ecto.UUID.generate()
      assert Organisation.fetch(uuid) == :not_found
    end

    test "found" do
      assert {:ok, %{id: id}} = Organisation.insert(@params)
      assert {:ok, org} = Organisation.fetch(id)
      assert org.name == @params.name
    end
  end

  describe "insert_user/2" do
    test "ok" do
      {:ok, organisation} = Fixture.organisation_params() |> Organisation.insert()
      {:ok, user} = Fixture.user_params() |> User.insert()
      {:ok, membership} = Organisation.insert_user(organisation, user)
      assert membership.user == user
      assert membership.organisation == organisation
      assert membership.type == :user
    end

    test "duplicate" do
      {:ok, organisation} = Fixture.organisation_params() |> Organisation.insert()
      {:ok, user} = Fixture.user_params() |> User.insert()
      {:ok, _membership} = Organisation.insert_user(organisation, user)
      assert {:error, changeset} = Organisation.insert_user(organisation, user)
      assert errors_on(changeset) == %{user: ["is already a member"]}
    end
  end

  describe "insert_admin/2" do
    test "ok" do
      {:ok, organisation} = Fixture.organisation_params() |> Organisation.insert()
      {:ok, user} = Fixture.user_params() |> User.insert()
      {:ok, membership} = Organisation.insert_admin(organisation, user)
      assert membership.user == user
      assert membership.organisation == organisation
      assert membership.type == :admin
    end
  end
end
