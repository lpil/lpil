# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# Configures the endpoint
config :fawkes, Fawkes.Endpoint,
  url: [host: "localhost"],
  root: Path.dirname(__DIR__),
  secret_key_base:
    "6X6MrQHhkt99YkBP+DC+VzVHoUI9Qs5PZJYVIlw+wp8fMLmT0+93VPgddLDJx7+J",
  render_errors: [accepts: ~w(html json)],
  pubsub: [name: Fawkes.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"

# Configure phoenix generators
config :phoenix, :generators,
  migration: true,
  binary_id: false

# Configure template engines
config :phoenix, :template_engines,
  slim: PhoenixSlime.Engine,
  slime: PhoenixSlime.Engine

# Configure Guardian auth framework
config :guardian, Guardian,
  allowed_algos: ["HS512"],
  verify_module: Guardian.JWT,
  issuer: "Fawkes",
  ttl: { 30, :days },
  verify_issuer: true,
  secret_key:
    "6X6MrQHhy199YkjP+D3+VzVH9UI9Qs5PZnYVIlh+wp8luLmT0+93Vigd+LDJx7Jz",
  serializer: Fawkes.GuardianSerializer
