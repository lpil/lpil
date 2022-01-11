use Mix.Config

config :particle, ecto_repos: [Particle.Repo]

config :particle, Particle.Repo, adapter: Ecto.Adapters.Postgres

case Mix.env() do
  :test ->
    config :particle, Particle.Repo,
      username: "postgres",
      password: "postgres",
      database: "particle_test",
      hostname: "localhost",
      pool: Ecto.Adapters.SQL.Sandbox

  :dev ->
    config :particle, Particle.Repo,
      username: "postgres",
      password: "postgres",
      database: "particle_dev",
      hostname: "localhost",
      pool_size: 10

  :prod ->
    # Creds are loaded with the `DATABASE_URL` env var
    config :particle, Particle.Repo,
      adapter: Ecto.Adapters.Postgres,
      database: "particle_prod",
      pool_size: 15
end
