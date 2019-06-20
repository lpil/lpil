defmodule Fawkes.QueryCase do
  @moduledoc """
  This module defines the test case to be used by
  query tests.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      alias Fawkes.Repo
      alias Fawkes.Factory
      import Ecto
      import Ecto.Query, only: [from: 1, from: 2]
      import Fawkes.QueryCase
    end
  end

  setup tags do
    unless tags[:async] do
      Ecto.Adapters.SQL.restart_test_transaction(Fawkes.Repo, [])
    end
    :ok
  end
end
