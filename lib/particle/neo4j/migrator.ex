defmodule Particle.Neo4j.Migrator do
  @moduledoc """
  Modifies the Neo4j schema on boot. Creates our constraints, etc.
  """
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, [])
  end

  def init(_) do
    cypher = """
    CREATE CONSTRAINT ON (user:User)
    ASSERT user.id IS UNIQUE;

    CREATE CONSTRAINT ON (user:User)
    ASSERT user.email IS UNIQUE;
    """

    {:ok, _} = Particle.Neo4j.query(cypher)

    # Shut down, we're done.
    :ignore
  end
end
