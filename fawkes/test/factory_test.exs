defmodule Fawkes.FactoryTest do
  use ExUnit.Case, async: false

  alias Fawkes.Factory

  setup _tags do
    Ecto.Adapters.SQL.restart_test_transaction(Fawkes.Repo, [])
    :ok
  end

  @factories ~w(user article)a

  for type <- @factories do

    test "the #{type} factory is valid, even for multiple inserts" do
      assert Factory.create(unquote(type))
      assert Factory.create(unquote(type))
    end

  end
end
