defmodule Particle.DbMigrator do
  @moduledoc """
  Modifies the database schema when the application instance starts,
  creating classes and constraints.

  https://orientdb.com/docs/last/SQL-Alter-Property.html
  """
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, [])
  end

  def init(_) do
    {:ok, _} = c("CREATE CLASS User IF NOT EXISTS")

    {:ok, _} = c("CREATE PROPERTY User.inserted_at IF NOT EXISTS Datetime")
    {:ok, _} = c("ALTER PROPERTY User.inserted_at DEFAULT 'sysdate()'")
    {:ok, _} = c("ALTER PROPERTY User.inserted_at MANDATORY TRUE")
    {:ok, _} = c("ALTER PROPERTY User.inserted_at NOTNULL TRUE")

    {:ok, _} = c("CREATE PROPERTY User.email IF NOT EXISTS String")
    {:ok, _} = c("ALTER PROPERTY User.email MANDATORY TRUE")
    {:ok, _} = c("ALTER PROPERTY User.email NOTNULL TRUE")
    {:ok, _} = c(~S(ALTER PROPERTY User.email REGEXP '.+@.+\\..+'))

    {:ok, _} = c("CREATE PROPERTY User.id IF NOT EXISTS String")
    {:ok, _} = c("ALTER PROPERTY User.id MANDATORY TRUE")
    {:ok, _} = c("ALTER PROPERTY User.id NOTNULL TRUE")
    {:ok, _} = c("ALTER PROPERTY User.id DEFAULT 'uuid()'")
    {:ok, _} = c("ALTER PROPERTY User.id READONLY TRUE")

    # https://github.com/orientechnologies/orientdb/issues/7529
    case c("CREATE INDEX User_email ON User (email) UNIQUE") do
      {:ok, _} -> nil
      {:duplicate_index, _} -> nil
    end

    # Shut down, we're done.
    :ignore
  end

  defp c(sql) do
    Orientdb.command(sql)
  end
end
