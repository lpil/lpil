defmodule BoilerplateWeb.ConnCase do
  @moduledoc """
  This module defines the test case to be used by
  tests that require setting up a connection.

  Such tests rely on `Phoenix.ConnTest` and also
  import other functionality to make it easier
  to build common datastructures and query the data layer.

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
      import BoilerplateWeb.Router.Helpers

      # The default endpoint for testing
      @endpoint BoilerplateWeb.Endpoint
    end
  end

  def session_conn() do
    secret_key_base = Application.get_env(:boilerplate, BoilerplateWeb.Endpoint)[:secret_key_base]

    session_opts =
      Application.get_env(:boilerplate, BoilerplateWeb.Endpoint)[:session_opts]
      |> Plug.Session.init()

    Phoenix.ConnTest.build_conn()
    |> Map.put(:secret_key_base, secret_key_base)
    |> Plug.Session.call(session_opts)
    |> Plug.Conn.fetch_session()
  end

  setup tags do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Boilerplate.Repo)

    unless tags[:async] do
      Ecto.Adapters.SQL.Sandbox.mode(Boilerplate.Repo, {:shared, self()})
    end

    {:ok, conn: session_conn()}
  end
end
