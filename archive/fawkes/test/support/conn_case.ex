defmodule Fawkes.ConnCase do
  @moduledoc """
  This module defines the test case to be used by
  tests that require setting up a connection.

  Such tests rely on `Phoenix.ConnTest` and also
  imports other functionality to make it easier
  to build and query models.

  Finally, if the test case interacts with the database,
  it cannot be async. For this reason, every test runs
  inside a transaction which is reset at the beginning
  of the test unless the test case is marked as async.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      # Import conveniences for testing with connections
      use Phoenix.ConnTest

      alias Fawkes.Repo
      alias Fawkes.Factory
      import Ecto
      import Ecto.Changeset
      import Ecto.Query, only: [from: 1, from: 2]

      import Fawkes.Router.Helpers

      # The default endpoint for testing
      @endpoint Fawkes.Endpoint


      def sign_in(%Plug.Conn{} = conn, user) do
        conn
        |> bypass_through(Fawkes.Router, [:browser])
        |> get("/")
        |> Guardian.Plug.sign_in(user, :token)
        |> send_resp(200, "Flush the session")
        |> recycle()
      end

      setup %{conn: conn} = config do
        attrs = config[:login_as]
        if attrs do
          user = Factory.create(:user)
          conn = sign_in(conn, user)
          {:ok, conn: conn, user: user}
        else
          :ok
        end
      end

    end
  end

  setup tags do
    unless tags[:async] do
      Ecto.Adapters.SQL.restart_test_transaction(Fawkes.Repo, [])
    end
    {:ok, conn: Phoenix.ConnTest.conn()}
  end
end
