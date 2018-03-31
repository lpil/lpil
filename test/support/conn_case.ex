defmodule ParticleWeb.ConnCase do
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
      import ParticleWeb.Router.Helpers

      # The default endpoint for testing
      @endpoint ParticleWeb.Endpoint
    end
  end

  setup _tags do
    {:ok, conn: session_conn()}
  end

  def session_conn() do
    secret_key_base = Application.get_env(:particle, ParticleWeb.Endpoint)[:secret_key_base]

    session_opts =
      Application.get_env(:particle, ParticleWeb.Endpoint)[:session_opts]
      |> Plug.Session.init()

    Phoenix.ConnTest.build_conn()
    |> Map.put(:secret_key_base, secret_key_base)
    |> Plug.Session.call(session_opts)
    |> Plug.Conn.fetch_session()
  end
end
