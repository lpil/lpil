defmodule Fawkes.SessionControllerTest do
  use Fawkes.ConnCase

  alias Fawkes.User

  # New

  test "GET new", %{conn: conn} do
    conn = get conn, "/session/new"
    assert html_response(conn, 200)
  end


  # Create

  test "POST create", %{conn: conn} do
    attrs = %{
      email: "foo@bar.baz",
      username: "foobar",
      password: "supersecret",
      password_confirmation: "supersecret",
    }
    user = %User{}
      |> User.registration_changeset(attrs)
      |> Repo.insert!()
    conn = post conn, "/session", session: %{
      username: "foobar", password: "supersecret"
    }
    assert redirected_to(conn) == "/"
    assert Guardian.Plug.current_resource(conn).id == user.id
  end

  test "POST create with unknown user", %{conn: conn} do
    conn = post conn, "/session", session: %{
      username: "hello", password: "1234567890"
    }
    body = html_response(conn, 200)
    assert body =~ "Invalid"
    assert body =~ "Log in"
    assert Guardian.Plug.current_resource(conn) == nil
  end

  test "POST create with incorrect password", %{conn: conn} do
    attrs = %{
      email: "foo@bar.baz",
      username: "foobar",
      password: "supersecret",
      password_confirmation: "supersecret",
    }
    %User{}
    |> User.registration_changeset(attrs)
    |> Repo.insert!()
    conn = post conn, "/session", session: %{
      username: "foobar", password: "1234567890"
    }
    body = html_response(conn, 200)
    assert body =~ "Invalid"
    assert body =~ "Log in"
    assert Guardian.Plug.current_resource(conn) == nil
  end
end
