use Mix.Config

config :boilerplate, ecto_repos: [Boilerplate.Repo]

config :boilerplate, Boilerplate.Repo, adapter: Ecto.Adapters.Postgres

case Mix.env() do
  :test ->
    config :boilerplate, Boilerplate.Repo,
      username: "postgres",
      password: "postgres",
      database: "boilerplate_test",
      hostname: "localhost",
      pool: Ecto.Adapters.SQL.Sandbox

  :dev ->
    config :boilerplate, Boilerplate.Repo,
      username: "postgres",
      password: "postgres",
      database: "boilerplate_dev",
      hostname: "localhost",
      pool_size: 10

  :prod ->
    # Creds are loaded with the `DATABASE_URL` env var
    config :boilerplate, Boilerplate.Repo,
      adapter: Ecto.Adapters.Postgres,
      database: "boilerplate_prod",
      pool_size: 15
end
