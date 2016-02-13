defmodule Fawkes.UserTest do
  use Fawkes.ModelCase

  alias Fawkes.User
  alias Comeonin.Bcrypt

  password = "0123456789"
  @attrs %{
    email: "louis@lpil.uk",
    username: "louis",
  }


  # changeset/2

  @tag :async
  test "changeset can be valid" do
    changeset = User.changeset(%User{}, @attrs)
    assert changeset.valid?
  end

  @tag :async
  test "changeset is invalid without email" do
    attrs = Dict.delete @attrs, :email
    changeset = User.changeset(%User{}, attrs)
    refute changeset.valid?
    assert [email: _] = changeset.errors
  end

  @tag :async
  test "email must look like an email" do
    attrs = Dict.put @attrs, :email, "123"
    changeset = User.changeset(%User{}, attrs)
    refute changeset.valid?
    assert [email: _] = changeset.errors
    attrs = Dict.put @attrs, :email, "a@b.c"
    changeset = User.changeset(%User{}, attrs)
    assert changeset.valid?
  end


  # registration_changeset/2

  @reg_attrs Map.merge @attrs, %{
    password: password,
    password_confirmation: password,
  }

  @tag :async
  test "registration_changeset can be valid" do
    changeset = User.changeset(%User{}, @reg_attrs)
    assert changeset.valid?
  end

  @tag :async
  test "registration changeset is invalid without password" do
    attrs = Dict.delete @reg_attrs, :password
    changeset = User.registration_changeset(%User{}, attrs)
    refute changeset.valid?
    assert [password: _] = changeset.errors
  end

  @tag :async
  test "registration changeset is invalid without password confirmation" do
    attrs = Dict.delete @reg_attrs, :password_confirmation
    changeset = User.registration_changeset(%User{}, attrs)
    refute changeset.valid?
    assert [password_confirmation: _] = changeset.errors
  end

  @tag :async
  test """
  registration_changeset is invalid unless password and confirmation match
  """ do
    attrs = Dict.put @reg_attrs, :password_confirmation, "foobar"
    refute attrs.password == attrs.password_confirmation
    changeset = User.registration_changeset(%User{}, attrs)
    refute changeset.valid?
    assert [password_confirmation: _] = changeset.errors
  end

  @tag :async
  test "passwords must be longish" do
    password = "123456789"
    attrs = Map.merge @reg_attrs, %{
      password: password, password_confirmation: password,
    }
    changeset = User.registration_changeset(%User{}, attrs)
    refute changeset.valid?
    assert [password: _] = changeset.errors
  end

  @tag :async
  test "passwords are hashed" do
    changeset = User.registration_changeset(%User{}, @reg_attrs)
    hash = Ecto.Changeset.get_change( changeset, :password_hash )
    refute @reg_attrs.password == hash
    assert Bcrypt.checkpw( @reg_attrs.password, hash )
  end

  @tag :async
  test "password_hash does not get set if password is nil" do
    attrs = Dict.delete @reg_attrs, :password
    changeset = User.registration_changeset(%User{}, attrs)
    refute Ecto.Changeset.get_change(changeset, :password_hash)
    refute changeset.valid?
  end

  @tag :async
  test "password_hash does not get set if password is not confirmed" do
    attrs = Dict.put @reg_attrs, :password_confirmation, "foobar"
    refute attrs.password == attrs.password_confirmation
    changeset = User.registration_changeset(%User{}, attrs)
    refute Ecto.Changeset.get_change(changeset, :password_hash)
    refute changeset.valid?
  end

  test "emails must be unique" do
    assert {:ok, _} =
      %User{}
      |> User.registration_changeset(@reg_attrs)
      |> Repo.insert
    attrs = %{ @reg_attrs | username: "hello" }
    assert {:error, changeset} =
      %User{}
      |> User.registration_changeset(attrs)
      |> Repo.insert
    assert changeset.errors == [
      email: "has already been taken",
    ]
  end

  test "usernames must be unique" do
    assert {:ok, _} =
      %User{}
      |> User.registration_changeset(@reg_attrs)
      |> Repo.insert
    attrs = %{ @reg_attrs | email: "foo@bar.com" }
    assert {:error, changeset} =
      %User{}
      |> User.registration_changeset(attrs)
      |> Repo.insert
    assert changeset.errors == [
      username: "has already been taken",
    ]
  end
end
