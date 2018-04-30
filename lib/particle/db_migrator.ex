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
    {:ok, _} =
      Orientdb.batch("""
      CREATE CLASS User IF NOT EXISTS;

      CREATE PROPERTY User.inserted_at IF NOT EXISTS Datetime;
      ALTER PROPERTY User.inserted_at DEFAULT 'sysdate()';
      ALTER PROPERTY User.inserted_at MANDATORY TRUE;
      ALTER PROPERTY User.inserted_at NOTNULL TRUE;

      CREATE PROPERTY User.email IF NOT EXISTS String;
      ALTER PROPERTY User.email MANDATORY TRUE;
      ALTER PROPERTY User.email NOTNULL TRUE;
      ALTER PROPERTY User.email REGEXP '.+@.+\\\\..+';

      CREATE PROPERTY User.id IF NOT EXISTS String;
      ALTER PROPERTY User.id MANDATORY TRUE;
      ALTER PROPERTY User.id NOTNULL TRUE;
      ALTER PROPERTY User.id DEFAULT 'uuid()';
      ALTER PROPERTY User.id READONLY TRUE
      """)

    # https://github.com/orientechnologies/orientdb/issues/7529
    case Orientdb.command("CREATE INDEX User_email ON User (email) UNIQUE") do
      {:ok, _} -> nil
      {:duplicate_index, _} -> nil
    end

    # Shut down, we're done.
    :ignore
  end
end
