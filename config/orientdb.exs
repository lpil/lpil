use Mix.Config

case Mix.env() do
  :dev ->
    config :particle, :orientdb,
      port: 2480,
      host: "localhost",
      db_name: "particle-dev",
      username: "root",
      password: "orientdb"

  :test ->
    config :particle, :orientdb,
      port: 2480,
      host: "localhost",
      db_name: "particle-test",
      username: "root",
      password: "orientdb"
end
