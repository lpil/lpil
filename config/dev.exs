use Mix.Config

# For development, we disable any cache and enable
# debugging and code reloading.
#
# The watchers configuration can be used to run external
# watchers to your application.
config :fawkes, Fawkes.Endpoint,
  http: [port: 4000],
  debug_errors: true,
  code_reloader: true,
  check_origin: false,
  watchers: [
    # Run a webpack watcher to compile the frontend
    node: [
      "node_modules/webpack/bin/webpack.js",
      "--watch",
      "--progress",
      "--colors",
    ]
  ]

# Watch static and templates for browser reloading.
config :fawkes, Fawkes.Endpoint,
  live_reload: [
    patterns: [
      ~r{priv/static/.*(js|css|png|jpeg|jpg|gif|svg)$},
      ~r{priv/gettext/.*(po)$},
      ~r{web/views/.*(ex)$},
      ~r{web/templates/.*(eex)$}
    ]
  ]

# Do not include metadata nor timestamps in development logs
config :logger, :console, format: "[$level] $message\n"

# Set a higher stacktrace during development.
# Do not configure such in production as keeping
# and calculating stacktraces is usually expensive.
config :phoenix, :stacktrace_depth, 20

# Configure your database
config :fawkes, Fawkes.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "postgres",
  password: "postgres",
  database: "fawkes_dev",
  hostname: "localhost",
  pool_size: 10

# Configure the auto test runner
config :mix_test_watch,
  tasks: ~w(test dogma)
