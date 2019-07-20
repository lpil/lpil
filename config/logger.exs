use Mix.Config

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:user_id]

case Mix.env() do
  :dev ->
    # Do not include metadata nor timestamps in development logs
    config :logger, :console, format: "[$level] $message\n"

  :prod ->
    # Do not print debug messages in production
    config :logger, level: :info

  :test ->
    # Print only warnings and errors during test
    config :logger, level: :warn
end
